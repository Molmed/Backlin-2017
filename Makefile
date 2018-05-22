clean:
	find . -name "*.log" | xargs rm
	rm -f all_jobs.err.log
	rm -f Rplots.pdf

.PHONY: dependencies all_jobs.err.log

dependencies:
	R -f install_dependencies.r

run: dependencies
	R -f benchmark.r

all_jobs.err.log:
	find . -path "*/output/*.err.log" -exec echo "{}" \; -exec cat {} \; -exec echo "--------" \; > $@

backup:
	find . -path "*/output/*.log" -print | zip backup.zip -@
	
