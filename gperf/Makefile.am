erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_eper)/src
erlsrc_DATA = gperf.erl gperfConsumer.erl gperfGtk.erl

bindir = $(ERLANG_INSTALL_LIB_DIR_eper)/priv/bin
dist_bin_SCRIPTS = gperf

erlgladedir = $(ERLANG_INSTALL_LIB_DIR_eper)/priv/glade
erlglade_DATA = gperf.gladep gperf.glade

erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_eper)/ebin
erlbeam_DATA = $(patsubst %.erl, %.beam, $(erlsrc_DATA))

EXTRA_DIST = $(erlsrc_DATA) $(dist_bin_SCRIPTS) $(erlglade_DATA)
CLEANFILES = $(erlbeam_DATA)

all: $(erlbeam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
