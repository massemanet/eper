erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_eper)/src
erlsrc_DATA = prfNet.erl prfSys.erl prfPrc.erl \
prf.erl prfHost.erl prfTarg.erl prfClient.erl

erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_eper)/ebin
erlbeam_DATA = $(patsubst %.erl, %.beam, $(erlsrc_DATA))

EXTRA_DIST = $(erlsrc_DATA) $(erlinc_DATA)
CLEANFILES = $(erlbeam_DATA)

all: $(erlbeam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
