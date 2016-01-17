#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>
#include <psapi.h>
#include <mutex>

struct dl_phdr_info
{
	void			 *dlpi_addr;
	const char       *dlpi_name;
};

int dl_iterate_phdr(int(*callback)(struct dl_phdr_info *info, size_t size, void *data),
					void *data)
{
	DWORD proc_id = GetCurrentProcessId();
	HANDLE proc_handle = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ,
									 FALSE, proc_id);
	if (NULL == proc_handle)
		return 0;

	int last_ret = 0;

	HMODULE modules[1024];
	DWORD used_size;

	if (EnumProcessModules(proc_handle, modules, sizeof(modules), &used_size))
	{
		for (unsigned int i = 0; i<used_size/sizeof(HMODULE); i++)
		{
			char mod_name[MAX_PATH];

			if (GetModuleFileNameExA(proc_handle, modules[i], mod_name,
									 sizeof(mod_name)))
			{
				dl_phdr_info  hdr;
				hdr.dlpi_name = mod_name;
				hdr.dlpi_addr = modules[i];

				last_ret = callback(&hdr, sizeof(hdr), data);
				if (last_ret != 0)
					break;
			}
		}
	}

	CloseHandle(proc_handle);

	return  last_ret;
}

uint8_t *getSectionDataPE(void *handle, const char *section_name,
                          unsigned long *section_size)
{
	unsigned char *pe = (unsigned char *)handle;

	int nt_headers_offset = pe[0x3C];

	bool  assert1 = pe[nt_headers_offset] == 'P' && pe[nt_headers_offset + 1] == 'E';
	if (assert1 == false)
	{
		return  nullptr;
	}

	unsigned char *coff = pe + nt_headers_offset + 4;

	int16_t    NumberOfSections = *(int16_t *)(coff + 2);

	// SizeOfOptionalHeader
	int16_t SizeOfOptionalHeader = *(int16_t *)(coff + 16);

	const int kCoffFileHeaderSize = 20;
	unsigned char *section_table_base = coff + kCoffFileHeaderSize + SizeOfOptionalHeader;

	// Section Header Record
	const int kSectionRecordSize = 40;

	unsigned char *section_header = section_table_base;
	for (int i = 0; i < NumberOfSections; i++)
	{
		uint32_t  VirtualSize = *(uint32_t *)&section_header[8];
		uint32_t  VirtualAddress = *(uint32_t *)&section_header[12];

		char name_of_this_section[9];
		memcpy(name_of_this_section, section_header, 8);
		name_of_this_section[8] = '\0';
		
		if (strcmp(section_name, name_of_this_section) == 0)
		{
			*section_size = VirtualSize;
			return  (uint8_t *)handle + VirtualAddress;
		}
		section_header += kSectionRecordSize;
	}

	return  nullptr;
}

namespace std {  

static std::unique_lock<std::mutex>* lock_ = nullptr;
static std::mutex mutex_;

template class function<void()>;
function<void()> __once_functor;

mutex&  __get_once_mutex()
{
	return mutex_;
}

void __set_once_functor_lock_ptr(unique_lock<mutex>* __ptr)
{
	lock_ = __ptr;
}

extern "C" void __once_proxy()
{
	function<void()> once_functor = std::move(__once_functor);
	unique_lock<mutex> *lock = lock_;
	if (lock != nullptr)
	{
		lock_ = nullptr;
		lock->unlock();
	}

	once_functor();
}

}
