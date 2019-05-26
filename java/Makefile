AUTOWEKA_URL = http://www.cs.ubc.ca/labs/beta/Projects/autoweka/autoweka-2.6.1.zip
RAUTOWEKA_JAR = RAutoWEKA.jar

default: all

.PHONY: all
all: dependencies build package install clean

.PHONY: dependencies
dependencies:
	mkdir lib
	curl -o lib/autoweka.zip '$(AUTOWEKA_URL)'
	unzip lib/autoweka.zip -d lib
	rm lib/autoweka.zip

.PHONY: build
build:
	mkdir bin
	javac -d bin -target 8 -source 8 -cp lib/autoweka.jar src/*.java

.PHONY: package
package:
	mkdir artifacts
	jar cMf artifacts/$(RAUTOWEKA_JAR) -C bin/ .

.PHONY: clean
clean:
	rm -rf lib
	rm -rf bin
	rm -rf artifacts

.PHONY: install
install:
	rm -rf ../inst/
	mkdir -p ../inst/java
	# AutoWEKA
	cp -a lib/autoweka.smac.SMACExperimentConstructor.properties ../inst/
	cp -aR lib/params ../inst/
	cp -aR lib/smac-v2.10.03-master-778 ../inst/java/
	rm -rf ../inst/java/smac-v2.10.03-master-778/doc
	rm -rf ../inst/java/smac-v2.10.03-master-778/example_scenarios
	rm -rf ../inst/java/smac-v2.10.03-master-778/example_scripts
	cp -a lib/autoweka.jar ../inst/java/
	# RAutoWEKA
	cp -a artifacts/$(RAUTOWEKA_JAR) ../inst/java/
