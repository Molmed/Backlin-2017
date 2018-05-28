clean:
	find . -name "*.log" | xargs rm
	rm -f all_jobs.err.log
	rm -f Rplots.pdf
	rm -f code.zip

.PHONY: dependencies all_jobs.err.log

dependencies:
	R --vanilla -f install_dependencies.R

benchmark: dependencies
	R --vanilla -f benchmark.R

all_jobs.err.log:
	find . -path "*/output/*.err.log" -exec echo "{}" \; -exec cat {} \; -exec echo "--------" \; > $@

backup:
	find . -path "*/output/*.log" -print | zip backup.zip -@
	
code-examples: dependencies
	R --vanilla -f code.R


code.zip:
	git archive --format zip --output $@ master
