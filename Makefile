# -*- mode: Makefile; -*- ��������� Makefile ���� ��� Emacs
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
# ��������� 3 ������ ����� �� �������
ifeq (,$(WORKDIR))
WORKDIR = $(shell pushd ../ 1>/dev/null && pwd )
endif

#�ӣ ��� �������� �������� ����� :) - ���������� ����������������
VERBOSEMAKE=""
#���� ������ -g ��� ������ �� ����� - ���� ������ � ����
#NDEBUG=""
#��� �����������
NOOPT=" "


#���������� ��� *.� ������
#CC_LOCAL := gcc
#���������� ��� *.cc � *.cpp ������
#CXX_LOCAL := g++
#���������� ��� *.f, *.for � *.F ������
#F77_LOCAL := g77
#������ ��� ����� ������
LD_LOCAL := g77

# �������������� ����� ��� ����������� C/C++
#COPTS  = #`root-config --cflags`

RELEASE := $(shell echo `cat $(WORKDIR)/.release`)
COMPDATE:= $(shell date)

# �������������� ����� ��� ����������� Fortran 
FOPTS  =  -g -fvxt -Wall -fno-automatic -finit-local-zero \
-fno-second-underscore -ffixed-line-length-120 -Wno-globals \
-DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_LNX -DCERNLIB_QMGLIBC -DCERNLIB_BLDLIB \
-DCOMPDATE="'$(COMPDATE)'" -I$(CERN)/pro/include -I$(CERN)/pro/include/geant321
#-fno-underscoring # ���� ������� �������� � ��������

# �������������� ����� ��� ������� 
LDOPTS = -Wl,-rpath -Wl,`root-config --libdir`#-shared #���� ������� ������������ ����������� ����������
#���� ���������� �������� CERNLIB, �� ����� ��������������� ���� ��������
CERNLIBRARY = "" 
#������ ���������, ���� ��� �� �������� ����������� ����� (��������� �� ����������)
# jetset74 mathlib graflib geant321 grafX11 packlib
CERNLIBS = geant jetset74 pawlib graflib grafX11 packlib mathlib

#���� ���������� ���������� ONLYBINARY, �� ���������� � ������ �����������
#ONLYBINARY=""
# ��� ���������� ����������� �����
BINDIR=../bin
#����������������, ���� ������� ������� ������������ ����������
#LIB_SHARED = ""

# �������������� ���� (����������� ����� ���)
LIB_LOCAL= `root-config --libs` -lstdc++ 

# ���������, ����� ��������� �� ����� ��������
# ����� ������� ��������� ������ ������������ ��� ������ ������, 
# ����� �� �������� bin - ��� ����������, ������� ������� ��� �������
# � ������������ ������� ���������� � ���������� example
#BINARIES = ks ks_i
BINARIES = ks 

# ������, �� ����� ������� ����� ������ ��� �������
# (��� ������ �� ����� �������� � ����������)
# � ����� ���������� ���� ���������� ��� ������
ks_MODULES   = kedrsim
ks_LIBS      = KedrGen KsGenQED KsGenPST KsGenKMD KrMu ReadNat bz2  KDB pq KcMagField KsNucInt
ks_i_MODULES = kedrsim_i
ks_i_LIBS    = KedrGen KsGenQED KsGenPST KsGenKMD KrMu ReadNat bz2  KDB pq KcMagField KsNucInt Xm Xt 

# ��������� ������ ����� �� �������
include $(WORKDIR)/KcReleaseTools/rules.mk








