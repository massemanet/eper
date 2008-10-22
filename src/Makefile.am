erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_eper)/src
erlincdir = $(ERLANG_INSTALL_LIB_DIR_eper)/src
gladedir = $(ERLANG_INSTALL_LIB_DIR_eper)/priv/glade
erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_eper)/ebin

erlsrc_DATA = \
	dtop.erl dtopConsumer.erl \
	gperf.erl gperfConsumer.erl gperfGtk.erl \
	ntop.erl ntopConsumer.erl \
	prfNet.erl prfSys.erl prfPrc.erl prfTrc.erl \
	prf.erl prfHost.erl prfTarg.erl prfConsumer.erl prf_crypto.erl \
	redbug.erl redbugConsumer.erl \
	sherk.erl sherk_tab.erl sherk_prof.erl \
	sherk_tree.erl sherk_list.erl sherk_scan.erl sherk_aquire.erl \
	sherk_target.erl sherk_ets.erl \
	watchdog.erl prfDog.erl

erlinc_DATA = \
	log.hrl

glade_DATA = \
	gperf.gladep gperf.glade \
	sherk.gladep sherk.glade

erlbeam_DATA = $(patsubst %.erl, %.beam, $(erlsrc_DATA))

EXTRA_DIST = $(erlsrc_DATA) $(erlinc_DATA) $(glade_DATA)
CLEANFILES = $(erlbeam_DATA)

all: $(erlbeam_DATA)

%.beam: %.erl
	$(ERLC) $(ERLCFLAGS) $<
