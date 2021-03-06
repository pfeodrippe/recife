clj_cmd = env clj

.PHONY: build
build:
	mkdir -p target
	$(clj_cmd) -X:depstar uberjar :jar target/recife.jar :sync-pom true :exclude '["clojure/.*", "lambdaisland/.*", "medley/.*"]'

.PHONY: deploy
deploy:
	mvn deploy:deploy-file -Dfile=target/recife.jar -DpomFile=pom.xml -DrepositoryId=clojars -Durl=https://clojars.org/repo/
