if [ -x aarch64/run_jit ]; then
    RUN="$(dirname $0)/aarch64/run_jit"
else
    if [ -x x86_64/run_jit ]; then
        RUN="$(dirname $0)/x86_64/run_jit"
    else
        RUN="$(dirname $0)/run"
    fi
fi
${RUN} "$(dirname $0)/umix.um"
