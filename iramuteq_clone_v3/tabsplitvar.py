# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os
import tempfile
from time import sleep

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ffr, FFF
from analysematrix import AnalyseMatrix
from functions import exec_rcode, check_Rresult
from dialog import FreqDialog
from PrintRScript import PrintRScript
from tableau import Tableau


class SplitMatrixFromVar(AnalyseMatrix):

    def doparametres(self, dlg=None) :
        if dlg is None :
            return
        else :
            dial = FreqDialog(self.parent, self.tableau.get_colnames(), "Column", size=(350, 200), showNA = False)
            dial.CenterOnParent()
            val = dial.ShowModal()
            if val == wx.ID_OK :
                self.parametres['colsel'] = dial.m_listBox1.GetSelections()
                self.parametres['header'] = dial.header
                self.parametres['tohistory'] = False
            else :
                self.parametres = None
            dial.Destroy()

    def doanalyse(self):
        newtabs = self.tableau.splitfromvar(self.parametres['colsel'][0])
        for mod in newtabs :
            tab = Tableau(self.ira, os.path.join(self.tableau.pathout['%s.csv' % mod]).replace('*',''))
            if not os.path.exists(tab.pathout.dirout) :
                os.mkdir(tab.pathout.dirout)
            tab.linecontent = newtabs[mod]
            tab.make_content_simple()
            tab.parametres['matrix'] = tab.parametres['uuid']
            self.ira.tree.OnItemAppend(tab.parametres, select = False)
