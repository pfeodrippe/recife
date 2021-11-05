clj_cmd = env clj

.PHONY: build
build:
	mkdir -p target
	$(clj_cmd) -X:depstar uberjar :jar target/recife.jar :sync-pom true :version '"0.4.0"' :exclude '["clojure/.*", "lambdaisland/.*", "medley/.*", "tla_edn/.*", "alandipert/.*", "metosin/.*", "quil/.*", "tlc2/.*", "com/.*", "javax/.*", "org/.*", "pcal/.*", "tla2sany/.*", "tla2tex/.*", "util/.*", "malli/.*", "jogamp/.*", "natives/.*", "processing/.*", "cljsjs/.*", "icon/.*", "fipp/.*", "edamame/.*", "javassist/.*", "arrudeia/.*"]'

.PHONY: deploy
deploy:
	mvn deploy:deploy-file -Dfile=target/recife.jar -DpomFile=pom.xml -DrepositoryId=clojars -Durl=https://clojars.org/repo/

.PHONY: test
test:
	clj -M:test

.PHONY: start-components
start-components:
	docker-compose -f docker/compose.yml pull
	docker-compose -f docker/compose.yml up -d

.PHONY: stop-components
stop-components:
	docker-compose -f docker/compose.yml down
