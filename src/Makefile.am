erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_eper)/src
erlincdir = $(ERLANG_INSTALL_LIB_DIR_eper)/src
gladedir = $(ERLANG_INSTALL_LIB_DIR_eper)/priv/glade
erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_eper)/ebin

erlsrc_DATA = \
	bread.erl bread_server.erl \
	dtop.erl dtopConsumer.erl dtop_replay.erl \
	gperf.erl gperfConsumer.erl gperfGtk.erl \
	gen_serv.erl \
	ntop.erl ntopConsumer.erl \
	prfNet.erl prfSys.erl prfPrc.erl prfTrc.erl prfDog.erl \
	prf.erl prfHost.erl prfTarg.erl prfConsumer.erl prf_crypto.erl \
	redbug.erl redbug_msc.erl redbugConsumer.erl \
	sherk.erl sherk_tab.erl sherk_prof.erl \
	sherk_tree.erl sherk_list.erl sherk_scan.erl sherk_aquire.erl \
	sherk_target.erl sherk_ets.erl \
	watchdog.erl watchdogConsumer.erl watchdogLogger.erl \
	replay_trc.erl \
	loggerConsumer.erl logReader.erl

erlinc_DATA = \
	log.hrl gen_serv.hrl

glade_DATA = \
	gperf.gladep gperf.glade \
	sherk.gladep sherk.glade

erlbeam_DATA = $(patsubst %.erl, %.beam, $(erlsrc_DATA))

EXTRA_DIST = $(erlsrc_DATA) $(erlinc_DATA) $(glade_DATA)
CLEANFILES = $(erlbeam_DATA)

all: $(erlbeam_DATA)

%.beam: %.erl
	$(ERLC) $(ERLCFLAGS) $<
