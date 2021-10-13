if [ -x aarch64/run_jit ]; then
    RUN="$(dirname $0)/aarch64/run_jit"
else
    RUN="$(dirname $0)/run"
fi
${RUN} "$(dirname $0)/umix.um"
