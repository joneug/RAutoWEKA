JAR = RAutoWEKA.jar

default: all

.PHONY: all
all: build package install clean

.PHONY: build
build:
	mkdir bin
	javac -d bin -target 8 -source 8 -cp ../inst/java/autoweka.jar src/*.java

.PHONY: package
package:
	mkdir artifacts
	jar cMf artifacts/$(JAR) -C bin/ .

.PHONY: clean
clean:
	rm -rf bin
	rm -rf artifacts

.PHONY: install
install:
	cp artifacts/$(JAR) ../inst/java/
