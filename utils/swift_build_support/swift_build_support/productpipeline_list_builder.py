# ===-- productpipeline_list_builder.py ----------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https:#swift.org/LICENSE.txt for license information
# See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===---------------------------------------------------------------------===#

import platform

from swift_build_support.swift_build_support import build_graph


class ProductPipeline(object):
    """Intermediate state object that is private to this file. We use it to maintain
       internally if our products are impl products are not. This is used for
       verification that we maintain this property in the product pipeline list
       builder.

       This class is meant to just be state.
    """
    def __init__(self, should_run_epilogue_operations, identity, is_impl):
        assert isinstance(identity, int)
        self.identity = identity
        self.products = []
        self.is_impl = is_impl
        self.should_run_epilogue_operations = should_run_epilogue_operations

    def append(self, product, is_enabled):
        self.products.append((product, is_enabled))

    def finalize(self):
        result = self.products
        self.products = []
        return result

    def __iter__(self):
        return iter(self.products)

    def __getitem__(self, i):
        return self.products[i]

    def __len__(self):
        return len(self.products)


class ProductPipelineListBuilder(object):
    """A builder object that constructs a list of product pipeline. A product
       pipeline is a list of products that are all meant to be inferred together
       and then run based on dependence scheduling from the build-graph. Each
       inferred list is produced in the same order in which one began each
       pipeline.

       NOTE: We require that a single product pipeline never contain both
       build-script products and build-script-impl products at the same
       time. This is important since build-script-impl pipelines first build all
       of the products, then test all of the products, then install all of the
       products. In contrast, build-script products always perform a
       build/test/install for each product in order.
    """
    def __init__(self, args):
        self.args = args
        self.current_count = 0
        self.current_pipeline = None
        self.is_current_pipeline_impl = False
        self.pipeline_list = []

    def begin_pipeline(self):
        if self.current_pipeline is not None:
            self.pipeline_list.append(self.current_pipeline)
        self.current_pipeline = ProductPipeline(False, identity=self.current_count,
                                                is_impl=False)
        self.current_count += 1
        self.is_current_pipeline_impl = False

    def begin_impl_pipeline(self, should_run_epilogue_operations=False):
        if self.current_pipeline is not None:
            self.pipeline_list.append(self.current_pipeline)
        self.current_pipeline = ProductPipeline(should_run_epilogue_operations,
                                                identity=self.current_count,
                                                is_impl=True)

        self.current_count += 1
        self.is_current_pipeline_impl = True

    def reset(self):
        self.current_count = 0
        self.current_pipeline = None
        self.is_current_pipeline_impl = False
        self.pipelinst_list = []

    def add_product(self, product_cls, is_enabled):
        """Add a non-impl product to the current pipeline begin constructed"""
        assert self.current_pipeline is not None
        assert not self.is_current_pipeline_impl
        assert not product_cls.is_build_script_impl_product()
        self.current_pipeline.append(product_cls, is_enabled)

    def add_impl_product(self, product_cls, is_enabled):
        """Add an impl product to the current pipeline begin constructed"""
        assert self.current_pipeline is not None
        assert self.is_current_pipeline_impl
        assert product_cls.is_build_script_impl_product()
        self.current_pipeline.append(product_cls, is_enabled)

    def infer(self):
        products_to_generation_index = {}
        enabled_products = set()
        inferred_pipeline_list = []
        last_impl_pipeline_index = None
        for i in range(len(self.pipeline_list)):
            pipeline = self.pipeline_list[i]
            if pipeline.is_impl:
                last_impl_pipeline_index = i
            final_pipeline = []
            for pipeline_i in range(len(pipeline)):
                (p, is_enabled) = pipeline[pipeline_i]
                # Make sure p has not been added multiple times to the builder.
                assert p not in products_to_generation_index
                products_to_generation_index[p] = (i, pipeline_i)
                if is_enabled:
                    final_pipeline.append(p)
                    enabled_products.add(p)
                else:
                    final_pipeline.append(None)
            inferred_pipeline_list.append(final_pipeline)

            # Go back through pipeline and make sure that all dependencies of
            # our product are from our generation or earlier. If we find such a
            # dependency error.
            for (p, is_enabled) in pipeline:
                assert (all(d in products_to_generation_index for d in
                            p.get_dependencies()))

        for i in range(len(inferred_pipeline_list)):
            pipeline = inferred_pipeline_list[i]

            # Filter out any of the pipelines that before inference were not
            # selected.
            enabled_pipeline = [p for p in pipeline if p is not None]

            if self.args.verbose_build:
                print("-- Build Graph Inference --")
                print("Initial Product List:")
                for p in enabled_pipeline:
                    print("    {}".format(p.product_name()))

            if len(enabled_pipeline) == 0:
                continue

            final_schedule = \
                build_graph.produce_scheduled_build(enabled_pipeline)[0]

            # Go through the schedule and remove all references to products that
            # we know are associated with an earlier pipeline. If it isn't from
            # an earlier pipeline or isn't in our current pipeline, through an
            # error. There is a dependency from our pipeline list
            is_darwin = platform.system() == 'Darwin'
            for p in final_schedule:
                if is_darwin and p.is_nondarwin_only_build_product():
                    continue
                (gen_offset, index) = products_to_generation_index[p]
                # If we are from an earlier generation, our position in the
                # inferred pipeline list may be None. Initialize it now.
                assert gen_offset <= i
                inferred_pipeline_list[gen_offset][index] = p

        filtered_results = []
        for pipeline in inferred_pipeline_list:
            filtered_results.append([p for p in pipeline if p is not None])

        if self.args.verbose_build:
            print("Final Build Order:")
            for pipeline in filtered_results:
                for p in pipeline:
                    print("    {}".format(p.product_name()))

        return (filtered_results, last_impl_pipeline_index)

    def finalize(self, shouldInfer):
        """Product a final schedule and return a list of our product pipelines. Resets
           the builder when done so is a consuming operation.
        """
        # Append the current pipeline if we have one.
        if self.current_pipeline is not None:
            self.pipeline_list.append(self.current_pipeline)

        result = None

        # Then if we are asked to do so run the inference algorithm on each
        # pipeline.
        if shouldInfer:
            result = self.infer()
        else:
            # Handle the is_enabled bit. When we infer, we want to before the
            # is_enabled after we register all of the generations.
            r = []
            last_index = None
            for i in range(len(self.pipeline_list)):
                if self.pipeline_list[i].is_impl:
                    last_index = i
                r.append([x[0] for x in self.pipeline_list[i]
                          if x[1] is True])
            result = (r, last_index)

        # Invalidates self.pipeline_list.
        self.reset()
        return result
