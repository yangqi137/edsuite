INSTALL_DIR = /usr/local/bin

include local.makefile

ALL = edsolver64z edsolver64d
edsolver64z: av_reader.o
edsolver64d: av_reader.o
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

