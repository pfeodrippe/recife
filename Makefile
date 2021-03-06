clj_cmd = env clj

.PHONY: build
build:
	mkdir -p target
	$(clj_cmd) -X:depstar uberjar :jar target/recife.jar :sync-pom true :version '"0.4.0-SNAPSHOT"' :exclude '["clojure/.*", "lambdaisland/.*", "medley/.*", "tla_edn/.*", "alandipert/.*", "metosin/.*", "quil/.*", "tlc2/.*", "com/.*", "javax/.*", "org/.*", "pcal/.*", "tla2sany/.*", "tla2tex/.*", "util/.*", "malli/.*", "jogamp/.*", "natives/.*", "processing/.*", "cljsjs/.*", "icon/.*", "fipp/.*", "edamame/.*"]'

.PHONY: deploy
deploy:
	mvn deploy:deploy-file -Dfile=target/recife.jar -DpomFile=pom.xml -DrepositoryId=clojars -Durl=https://clojars.org/repo/

.PHONY: test
test:
	clj -M:test
