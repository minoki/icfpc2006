if [ -x aarch64/run_jit ]; then
    RUN=aarch64/run_jit
else
    RUN=run
fi
${RUN} --input hmonk --input COMEFROM umix.um
