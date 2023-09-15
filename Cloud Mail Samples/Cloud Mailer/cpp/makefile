#
# Cloud Mail 2022 .NET Edition - Sample Project
#
# This sample project demonstrates the usage of Cloud Mail in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
#
# www.nsoftware.com/cloudmail
#
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
#

ifeq ($(shell uname -s), Darwin) # MACOS
R_PATH=-Wl,-rpath,../../lib64/,-rpath,.

FRAMEWORK = -framework Carbon -framework Security
LD_FLAGS = -lcloudmail.22.0 -L../../lib64/ -lresolv
LD_FLAGS_SRC = -lz -lresolv
OS_CFLAGS = -D UNIX -arch arm64
MACOS = "darwin% Darwin% macos%"

all:
	g++ $(OS_CFLAGS) -D UNIX -o cloudmailer cloudmailer.cpp  -I../../include/ $(LD_FLAGS) $(FRAMEWORK) $(R_PATH)

src: ../../src/cloudmail.o
	g++ $(OS_CFLAGS) -o cloudmailer ../../src/cloudmail.o cloudmailer.cpp  $(LD_FLAGS_SRC) $(FRAMEWORK)

../../src/cloudmail.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -arch arm64 -o ../../src/cloudmail.o ../../src/cloudmail.cpp

else # LINUX
ifeq ($(shell uname -m), x86_64)
  LIB=lib64
else
  LIB=lib
endif

R_PATH=-Wl,-rpath,../../$(LIB)/,-rpath,.
LD_FLAGS = -lcloudmail  -L../../$(LIB)/ 
LD_FLAGS_SRC = -lz 

all:
	g++ -D UNIX -o cloudmailer cloudmailer.cpp  -I../../include/ $(LD_FLAGS) $(R_PATH) 

src: ../../src/cloudmail.o
	g++ -D UNIX -o cloudmailer ../../src/cloudmail.o cloudmailer.cpp  -I../../include/ $(LD_FLAGS_SRC)

../../src/cloudmail.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -o ../../src/cloudmail.o ../../src/cloudmail.cpp

clean:
	rm -f ../../src/*.o
	rm -f cloudmailer *.o
endif