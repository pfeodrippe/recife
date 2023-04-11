clj_cmd = env clojure

.PHONY: build
build:
	mkdir -p target
	rm -rf classes
	mkdir -p classes
	clojure -e "(compile 'recife.class)"
	$(clj_cmd) -X:depstar uberjar :jar target/recife.jar :sync-pom true :version '"0.14.0"' :exclude '["clojure/.*", "lambdaisland/.*", "medley/.*", "alandipert/.*", "metosin/.*", "quil/.*", "tlc2/.*", "com/.*", "javax/.*", "org/.*", "pcal/.*", "tla2sany/.*", "tla2tex/.*", "util/.*", "malli/.*", "jogamp/.*", "natives/.*", "processing/.*", "cljsjs/.*", "icon/.*", "fipp/.*", "edamame/.*", "javassist/.*", "arrudeia/.*"]' :compile-ns '[recife.class]'

.PHONY: deploy
deploy:
	mvn deploy:deploy-file -Dfile=target/recife.jar -DpomFile=pom.xml -DrepositoryId=clojars -Durl=https://clojars.org/repo/

.PHONY: test
test:
	clojure -M:test

.PHONY: start-components
start-components:
	docker-compose -f docker/compose.yml pull
	docker-compose -f docker/compose.yml up -d

.PHONY: stop-components
stop-components:
	docker-compose -f docker/compose.yml down
