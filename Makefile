.PHONY: all clean run help

LIBPATH = lib
LIBS = scalatest_2.10-2.1.3 \
	netty-all-5.0.0.Alpha1
# dependencies
CLASSPATH = $(subst $(EMPTY) $(EMPTY),:,$(addprefix $(LIBPATH)/,$(addsuffix .jar,$(LIBS))))

# sources
SRCS = $(shell find src/main/scala -name *.scala)
TESTSRCS = $(shell find src/test/scala -name *.scala)
ALLSRCS = $(SRCS) $(TESTSRCS)

BINDIR = bin
DEPDIR = deps

# fake binary
BIN = $(DEPDIR)/build.dep
FULLBIN = $(DEPDIR)/full_build.dep

# main program class
CLASS = nnsearch.Program
# namespace of unit tests
TESTNS = unittest

# let's try to find names of specifications we have
SPECS = $(shell grep -r -i -P "class\s+((\w|\d)+)\s+extends\s+FlatSpec" src/test/scala | awk '{ print $$3}')

# names of unit test classes
TESTCLASS = $(addprefix $(TESTNS).,$(SPECS))

all: help $(FULLBIN)
	@echo "Running all test specifications we have ...."
	@scala -classpath "$(BINDIR):$(CLASSPATH)" org.scalatest.run $(TESTCLASS)

$(SPECS): help $(FULLBIN)
	@echo "Running spec $@.................."
	@scala -classpath "$(BINDIR):$(CLASSPATH)" org.scalatest.run unittest.$@

run: $(BIN)
	@echo "Server is up and running ..................."
	@scala -classpath "$(BINDIR):$(CLASSPATH)" $(CLASS) dict.txt

$(BINDIR):
	@mkdir -p $(BINDIR)

$(BIN): $(BINDIR) $(SRCS) $(DEPDIR) Makefile
	@echo "Building...................................."
	@fsc -feature -classpath "$(CLASSPATH)" -sourcepath src -deprecation -d $(BINDIR) $(SRCS)
	@touch $(BIN)

$(FULLBIN): $(BINDIR) $(ALLSRCS) $(DEPDIR) Makefile
	@echo "Building...................................."
	@fsc -feature -classpath "$(CLASSPATH)" -sourcepath src -deprecation -d $(BINDIR) $(ALLSRCS)
	@touch $(FULLBIN)

$(DEPDIR):
	@mkdir -p $(DEPDIR)

clean:
	@rm -rf $(BINDIR)
	@rm -rf $(DEPDIR)

help:
	@echo "make - builds & run all unit tests"
	@echo "make run - builds & run program"
	@echo "make SPEC - builds & run specific unit test"
	@echo "Available specs:"
	@echo -e $(addprefix "\\t- ",$(addsuffix \\n,$(SPECS)))
