clean:
	find . -name "*.log" | xargs rm
	rm -f all_jobs.err.log

.PHONY: dependencies all_jobs.err.log

dependencies:
	R -f install_dependencies.r

run: dependencies
	R -f benchmark.r

all_jobs.err.log:
	find . -path "*/output/*.err.log" -exec echo "{}" \; -exec cat {} \; -exec echo "--------" \; > $@
