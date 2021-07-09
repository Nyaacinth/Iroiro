define yue_compilation =
	
	if hash yue 2>/dev/null; then
		echo -e "\033[1m[\033[32mINFO\033[0m\033[1m] >>> \c"
		yue -v
		echo -e "\033[0m\c"
	else
		echo -e "\033[1m[\033[31mERROR\033[0m\033[1m] >>> Yue Compiler is required, but not found. Aborting...\033[0m"
		exit 255
	fi

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
	yue -m -t ${EXPORT_PATH} ${SOURCE_PATH}
	YUE_COMPLICATION_EXIT_CODE=$?
	if [ ! ${YUE_COMPLICATION_EXIT_CODE} = 0 ]; then
		echo -e "\033[1m[\033[31mERROR\033[0m\033[1m] >>> Something went wrong, please check your configuration. (::${YUE_COMPLICATION_EXIT_CODE})\033[0m"
		exit
	fi
	compilation_task_end_time=$(date "+%s")
	echo -e "\033[1m[\033[32mINFO\033[0m\033[1m] >>> Task Finished! (finished in $((compilation_task_end_time - compilation_task_start_time)) seconds)\033[0m"

endef

default: ; $(value yue_compilation)

# flags
.ONESHELL:
.SILENT:
# end_flags
