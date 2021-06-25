define yue_compilation =

	SOURCE_PATH=source
	EXPORT_PATH=export

	if [ ! -d "${SOURCE_PATH}" ]; then
		echo -e "\033[1m[\033[31mERROR\033[0m\033[1m] >>> Cannot access source directory, compilation terminated.\033[0m"
		exit 2
	fi
	if [ -d "${EXPORT_PATH}" ]; then
		if [ ! "`ls -A ${EXPORT_PATH}`" = "" ]; then
			echo -e "\033[1m[\033[32mINFO\033[0m\033[1m] >>> Cleaning up...\033[0m"
			rm -rf ${EXPORT_PATH}
		fi
	fi
	mkdir -p ${EXPORT_PATH}
	echo -e "\033[1m[\033[32mINFO\033[0m\033[1m] >>> Compilation Task: ${SOURCE_PATH} -> ${EXPORT_PATH}\033[0m"
	compilation_task_start_time=$(date "+%s")
	yue -t ${EXPORT_PATH} ${SOURCE_PATH}
	if [ ! $? = 0 ]; then
		echo -e "\033[1m[\033[31mERROR\033[0m\033[1m] >>> Something went wrong, please check your sources.\033[0m"
		exit 255
	fi
	compilation_task_end_time=$(date "+%s")
	echo -e "\033[1m[\033[32mINFO\033[0m\033[1m] >>> Task Finished! Takes $((compilation_task_end_time - compilation_task_start_time)) seconds\033[0m"

endef

default: ; $(value yue_compilation)

# flags
.ONESHELL:
.SILENT:
# end_flags

