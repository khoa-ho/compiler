
.PHONY: all test clean

# Build
all:
	jbuilder build @install

# Build and run tests
test:
	jbuilder runtest

# Clean up
clean:
	jbuilder clean