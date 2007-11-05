AC_INIT(Various Erlang performance related tools, 0.38, 
             mats.cronqvist@ericsson.com, eper)
dnl Require autoconf version >=2.59c. first one with erlang macros
AC_PREREQ(2.59c)
AC_COPYRIGHT(Copyright (C) 2006 Mats Cronqvist)
dnl AC_REVISION($Id$)

dnl Require automake version >=1.9.5.
AM_INIT_AUTOMAKE(1.9.5)

dnl C-related configuration.
AC_PROG_CC
AC_STDC_HEADERS

dnl Erlang-related configuration.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB(erl_interface)
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_TARNAME, AC_PACKAGE_VERSION)
erlcflags="+debug_info"
AC_SUBST([ERLCFLAGS], [$erlcflags])

dnl Enable doc link generation, iff not "no"
AC_ARG_ENABLE(
    [gtkdoclinks],
    [AS_HELP_STRING([--enable-gtkdoclinks=URL],
        [in the generated doc, generate links to the GTK+ docs (default is no);
        URL must begin with file:// (local) or http:// (www);
        URL should be set such that URL/gtk/ix01.html exists;
        for www, the http_proxy variable must be set properly])],
    [_gtkdoclinks=$enableval],
    [_gtkdoclinks=no])

AC_SUBST([GTKDOCLINKS], [$_gtkdoclinks])

dnl Specify the files to instantiate.
AC_CONFIG_FILES([ \
	Makefile \
	src/Makefile \
	bin/Makefile \
	])

AC_OUTPUT
