erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_eper)/src
erlsrc_DATA = sherk.erl sherk_tab.erl sherk_prof.erl \
sherk_tree.erl sherk_list.erl sherk_scan.erl sherk_aquire.erl \
sherk_target.erl sherk_ets.erl

gladedir = $(ERLANG_INSTALL_LIB_DIR_eper)/priv/glade
glade_DATA = sherk.gladep sherk.glade

bindir = $(ERLANG_INSTALL_LIB_DIR_eper)/priv/bin
dist_bin_SCRIPTS = sherk

erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_eper)/ebin
erlbeam_DATA = $(patsubst %.erl, %.beam, $(erlsrc_DATA))

EXTRA_DIST = $(erlsrc_DATA) $(glade_DATA)
CLEANFILES = $(erlbeam_DATA)

all: $(erlbeam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
