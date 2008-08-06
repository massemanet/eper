AC_INIT(Various Erlang performance related tools, 0.44,
             mats.cronqvist()gmail.com, eper)

dnl Require autoconf version >=2.59c. first one with erlang macros
AC_PREREQ(2.59c)
AC_COPYRIGHT(Copyright (C) 2008 Mats Cronqvist)
dnl AC_REVISION($Id$)

dnl Require automake version >=1.9.5.
AM_INIT_AUTOMAKE(1.9.5)

dnl Erlang-related configuration.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB(erl_interface)
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_TARNAME, AC_PACKAGE_VERSION)
erlcflags="+debug_info"
AC_SUBST([ERLCFLAGS], [$erlcflags])

dnl Specify the files to instantiate.
AC_CONFIG_FILES([ \
	Makefile \
	src/Makefile \
	bin/Makefile \
	])

AC_OUTPUT
