if [ -x aarch64/run_jit ]; then
    RUN=aarch64/run_jit
else
    RUN=run
fi
${RUN} --discard-initial-output --input howie --input xyzzy umix.um
