// RUN: %target-swift-frontend -Xllvm -tf-dynamic-compilation -Xllvm -tf-dump-graph -Xllvm -tf-module-level-graph=false -O -emit-sil %s | %FileCheck %s

import TensorFlow

@TensorFlowGraph
public func add(x: Tensor<Float>, y: Tensor<Float>) -> Tensor<Float> {
  return x + y
}

// CHECK-LABEL: --- TFPartition GraphDef Proto:
// CHECK: library {
// CHECK:   function {
// CHECK:     signature {
// CHECK:       name: "S26accelerator_only_functions3add1x1y10TensorFlow0E0VySfGAH_AHtF.tf_only"
// CHECK:       input_arg {
// CHECK:         name: "arg_0"
// CHECK:         type: DT_FLOAT
// CHECK:       }
// CHECK:       input_arg {
// CHECK:         name: "arg_1"
// CHECK:         type: DT_FLOAT
// CHECK:       }
// CHECK:       output_arg {
// CHECK:         name: "op_add_x_y___2_6_13__job_localhost_replica_0_task_0_device_cpu_0"
// CHECK:         type: DT_FLOAT
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/add.x_y_..6.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Identity"
// CHECK:       input: "arg_1"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/add.x_y_._1.6.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Identity"
// CHECK:       input: "arg_0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/add.x_y_._2.6.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Add"
// CHECK:       input: "op/add.x_y_._1.6.13_/job_localhost/replica_0/task_0/device_CPU_0:output:0"
// CHECK:       input: "op/add.x_y_..6.13_/job_localhost/replica_0/task_0/device_CPU_0:output:0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     ret {
// CHECK:       key: "op_add_x_y___2_6_13__job_localhost_replica_0_task_0_device_cpu_0"
// CHECK:       value: "op/add.x_y_._2.6.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:     }
// CHECK:   }
// CHECK: }
// CHECK: versions {
// CHECK:   producer: 27
// CHECK:   min_consumer: 12
// CHECK: }

@TensorFlowGraph
public func loop(x: Tensor<Float>) -> Tensor<Float> {
  var y = x + 1
  for i in 1...10 {
    y += y
  }
  return y
}

// CHECK-LABEL: --- TFPartition GraphDef Proto:
// CHECK: library {
// CHECK:   function {
// CHECK:     signature {
// CHECK:       name: "S26accelerator_only_functions4loop1x10TensorFlow0E0VySfGAG_tF.tf_only"
// CHECK:       input_arg {
// CHECK:         name: "arg_0"
// CHECK:         type: DT_FLOAT
// CHECK:       }
// CHECK:       output_arg {
// CHECK:         name: "op_loop_x___12_77_13__job_localhost_replica_0_task_0_device_cpu_0"
// CHECK:         type: DT_FLOAT
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_..77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Identity"
// CHECK:       input: "arg_0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_._1.77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Const"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "dtype"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:       attr {
// CHECK:         key: "value"
// CHECK:         value {
// CHECK:           tensor {
// CHECK:             dtype: DT_FLOAT
// CHECK:             tensor_shape {
// CHECK:             }
// CHECK:             float_val: 1
// CHECK:           }
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_._2.77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Add"
// CHECK:       input: "op/loop.x_..77.13_/job_localhost/replica_0/task_0/device_CPU_0:output:0"
// CHECK:       input: "op/loop.x_._1.77.13_/job_localhost/replica_0/task_0/device_CPU_0:output:0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_._3.77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Add"
// CHECK:       input: "op/loop.x_._2.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       input: "op/loop.x_._2.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_._4.77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Add"
// CHECK:       input: "op/loop.x_._3.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       input: "op/loop.x_._3.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_._5.77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Add"
// CHECK:       input: "op/loop.x_._4.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       input: "op/loop.x_._4.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_._6.77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Add"
// CHECK:       input: "op/loop.x_._5.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       input: "op/loop.x_._5.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_._7.77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Add"
// CHECK:       input: "op/loop.x_._6.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       input: "op/loop.x_._6.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_._8.77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Add"
// CHECK:       input: "op/loop.x_._7.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       input: "op/loop.x_._7.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_._9.77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Add"
// CHECK:       input: "op/loop.x_._8.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       input: "op/loop.x_._8.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_._10.77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Add"
// CHECK:       input: "op/loop.x_._9.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       input: "op/loop.x_._9.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_._11.77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Add"
// CHECK:       input: "op/loop.x_._10.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       input: "op/loop.x_._10.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     node_def {
// CHECK:       name: "op/loop.x_._12.77.13_/job_localhost/replica_0/task_0/device_CPU_0"
// CHECK:       op: "Add"
// CHECK:       input: "op/loop.x_._11.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       input: "op/loop.x_._11.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:       device: "/job:localhost/replica:0/task:0/device:CPU:0"
// CHECK:       attr {
// CHECK:         key: "T"
// CHECK:         value {
// CHECK:           type: DT_FLOAT
// CHECK:         }
// CHECK:       }
// CHECK:     }
// CHECK:     ret {
// CHECK:       key: "op_loop_x___12_77_13__job_localhost_replica_0_task_0_device_cpu_0"
// CHECK:       value: "op/loop.x_._12.77.13_/job_localhost/replica_0/task_0/device_CPU_0:z:0"
// CHECK:     }
// CHECK:   }
// CHECK: }
// CHECK: versions {
// CHECK:   producer: 27
// CHECK:   min_consumer: 12
// CHECK: }
