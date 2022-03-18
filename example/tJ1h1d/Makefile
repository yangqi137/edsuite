INSTALL_DIR = ../../../bin

include local.makefile

ALL = tJ1h1d
all: $(ALL)
%:%.o
	$(CXX) -o $@ $(LDFLAGS) $^ $(LDLIBS)
clean:
	rm *.o $(ALL)
dep:
	rm -f .depend
	make .depend
install: $(ALL)
	install $(ALL) $(INSTALL_DIR)
.depend:
	echo # Program dependencies > .depend
	$(CXX) $(CPPFLAGS) -MM *.cpp >> .depend
include .depend

