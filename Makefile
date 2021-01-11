clj_cmd = env clj -O:default-options

.PHONY: build
build:
	mkdir -p target
	$(clj_cmd) -A:depstar -m hf.depstar.uberjar target/recife.jar

.PHONY: deploy
deploy:
	mvn deploy:deploy-file -Dfile=target/recife.jar -DpomFile=pom.xml -DrepositoryId=clojars -Durl=https://clojars.org/repo/
