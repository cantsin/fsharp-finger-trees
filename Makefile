.PHONY: all test

all:
	xbuild /verbosity:quiet src/fingertrees/fingertrees.fsproj

test:
	xbuild /verbosity:quiet tests/fingertrees.Tests/fingertrees.Tests.fsproj && \
	mono packages/NUnit.Runners/tools/nunit-console.exe -labels -nodots tests/fingertrees.Tests/bin/Debug/fingertrees.Tests.dll
