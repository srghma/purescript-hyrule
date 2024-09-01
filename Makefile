# spago run --main Test.Examples.PollUnoptimized.DoublePoll
run_test_optimized:
	spago build
	purs-backend-es build --directives .purs-backend-es.dirictives
	./purs-backend-es-build-run.sh Test.Examples.PollUnoptimized.DoublePoll
	# ./purs-backend-es-build-run.sh Test.Main

