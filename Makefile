.PHONY: all clean run help

LIBPATH = libs
LIBS = scalatest_2.10-2.1.3 \
	netty-all-5.0.0.Alpha1
# dependencies
CLASSPATH = $(subst $(EMPTY) $(EMPTY),:,$(addprefix $(LIBPATH)/,$(addsuffix .jar,$(LIBS))))

# sources
SRCS = $(shell find src -name *.scala)
BINDIR = bin
DEPDIR = deps

# fake binary
BIN = $(DEPDIR)/build.dep

# main program class
CLASS = nnsearch.Program
# namespace of unit tests
TESTNS = unittest

# let's try to find names of specifications we have
SPECS = $(shell grep -r -i -P "class\s+((\w|\d)+)\s+extends\s+FlatSpec" src | awk '{ print $$3}')

# names of unit test classes
TESTCLASS = $(addprefix $(TESTNS).,$(SPECS))

all: help $(BIN)
	@echo "Running all test specifications we have ...."
	@scala -classpath "$(BINDIR):$(CLASSPATH)" org.scalatest.run $(TESTCLASS)

$(SPECS): help $(BIN)
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
