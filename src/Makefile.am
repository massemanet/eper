erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_eper)/src

erlsrc_DATA = ntop.erl ntopConsumer.erl \
	dtop.erl dtopConsumer.erl \
	gperf.erl gperfConsumer.erl gperfGtk.erl \
	gperf.erl gperfConsumer.erl gperfGtk.erl \
	prfNet.erl prfSys.erl prfPrc.erl prfTrc.erl \
	prf.erl prfHost.erl prfTarg.erl prfClient.erl \
	prfNet.erl prfSys.erl prfPrc.erl prfTrc.erl \
	prf.erl prfHost.erl prfTarg.erl prfClient.erl \
	prfNet.erl prfSys.erl prfPrc.erl prfTrc.erl \
	prf.erl prfHost.erl prfTarg.erl prfClient.erl \
	redbug.erl redbugConsumer.erl \
	sherk.erl sherk_tab.erl sherk_prof.erl \
	sherk_tree.erl sherk_list.erl sherk_scan.erl sherk_aquire.erl \
	sherk_target.erl sherk_ets.erl

gladedir = $(ERLANG_INSTALL_LIB_DIR_eper)/priv/glade
glade_DATA = gperf.gladep gperf.glade \
	sherk.gladep sherk.glade

erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_eper)/ebin
erlbeam_DATA = $(patsubst %.erl, %.beam, $(erlsrc_DATA))

EXTRA_DIST = $(erlsrc_DATA)
CLEANFILES = $(erlbeam_DATA)

all: $(erlbeam_DATA)

%.beam: %.erl
	echo $(PWD)
	$(ERLC) $(ERLCFLAGS) $<
