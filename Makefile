# -*- mode: Makefile; -*- установка Makefile моды для Emacs
#--------------------------------------------------------------------------
#      $Id: Makefile,v 1.32 2020/01/20 11:55:05 malysh Exp $
#
# Description:
#      Makefile for KcTemplate package
#
# Environment:
#      Software developed for the KEDR Detector at BINP.
#
# Author:
#      Baldin Evgueni Mihilovich (E.M.Baldin@inp.nsk.su)
#
# Copyright Information:
#      Copyright (C) 2000-2004  Budker's Institute of Nuclear Physics
#
#--------------------------------------------------------------------------
# следующие 3 строки лучше не трогать
ifeq (,$(WORKDIR))
WORKDIR = $(shell pushd ../ 1>/dev/null && pwd )
endif

#Всё что делается делается вслух :) - желательно раскоментировать
VERBOSEMAKE=""
#Если ключик -g при сборки не нужен - если уверен в себе
#NDEBUG=""
#нет оптимизации
NOOPT=" "


#Компилятор для *.с файлов
#CC_LOCAL := gcc
#Компилятор для *.cc и *.cpp файлов
#CXX_LOCAL := g++
#Компилятор для *.f, *.for и *.F файлов
#F77_LOCAL := g77
#Линкер для этого пакета
LD_LOCAL := g77

# Дополнительные опции для компилятора C/C++
#COPTS  = #`root-config --cflags`

RELEASE := $(shell echo `cat $(WORKDIR)/.release`)
COMPDATE:= $(shell date)

# Дополнительные опции для компилятора Fortran 
FOPTS  =  -g -fvxt -Wall -fno-automatic -finit-local-zero \
-fno-second-underscore -ffixed-line-length-120 -Wno-globals \
-DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_LNX -DCERNLIB_QMGLIBC -DCERNLIB_BLDLIB \
-DCOMPDATE="'$(COMPDATE)'" -I$(CERN)/pro/include -I$(CERN)/pro/include/geant321
#-fno-underscoring # если достали подчерки в фортране

# Дополнительные опции для линкера 
LDOPTS = -Wl,-rpath -Wl,`root-config --libdir`#-shared #если хочется использовать разделяемую библиотеку
#Если необходимо добавить CERNLIB, то лучше воспользоваться этим ключиком
CERNLIBRARY = "" 
#Список библиотек, если вам не нравится стандартный набор (расширяем по требованию)
# jetset74 mathlib graflib geant321 grafX11 packlib
CERNLIBS = geant jetset74 pawlib graflib grafX11 packlib mathlib

#Если определена переменная ONLYBINARY, то библиотека в пакете отсутствует
#ONLYBINARY=""
# где вываливать исполняемые файлы
BINDIR=../bin
#Раскомментируйте, если хочется собрать динамическую библиотеку
#LIB_SHARED = ""

# Дополнительные либы (вставляются после либ)
LIB_LOCAL= `root-config --libs` -lstdc++ 

# Определим, какие программы мы будем собирать
# Здесь следует указывать только используемые при работе пакеты, 
# чтобы не засорять bin - все программки, которые пишутся дли примера
# и тестирования следует складывать в директорию example
#BINARIES = ks ks_i
BINARIES = ks 

# укажем, из каких модулей этого пакета они состоят
# (эти модули не будут включены в библиотеку)
# и какие библиотеки надо подключить при сборке
ks_MODULES   = kedrsim
ks_LIBS      = KedrGen KsGenQED KsGenPST KsGenKMD KrMu ReadNat bz2  KDB pq KcMagField KsNucInt
ks_i_MODULES = kedrsim_i
ks_i_LIBS    = KedrGen KsGenQED KsGenPST KsGenKMD KrMu ReadNat bz2  KDB pq KcMagField KsNucInt Xm Xt 

# следующую строку лучше не трогать
include $(WORKDIR)/KcReleaseTools/rules.mk








