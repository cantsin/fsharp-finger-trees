.PHONY: all test

all:
	xbuild /verbosity:quiet src/fingertrees/fingertrees.fsproj

test:
	xbuild /verbosity:quiet tests/fingertrees.Tests/fingertrees.Tests.fsproj && \
	mono packages/NUnit.Runners/tools/nunit-console.exe -labels -nodots tests/fingertrees.Tests/bin/Debug/fingertrees.Tests.dll

# testing C# integration and compilation manually
test-interop:
	mcs -r:bin/fingertrees.dll src/interop/Interop.cs -r:packages/FAKE/tools/FSharp.Core.dll && mkbundle -o interop src/interop/Interop.exe --deps bin/fingertrees.dll && ./interop
