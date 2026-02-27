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
import json
import datetime
from time import sleep
from uuid import uuid4

import langue
langue.run()

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ConstructPathOut, simipath, ffr, PathOut
from functions import print_liste, exec_rcode, read_list_file, check_Rresult, indices_simi, treat_var_mod, normpath_win32
from dialog import SelectColDial, FreqDialog
from analysematrix import AnalyseMatrix
from listlex import *
from configparser import RawConfigParser


class Categorisation(AnalyseMatrix):

    def doparametres(self, dlg = None) :
        self.listactives = self.parametres.get('listactives', False)
        self.actives = self.parametres.get('actives', False)
        self.cmd = self.parametres.get('cmd', False)
        self.dirout = self.parametres.get('pathout', False)
        self.Source = None
        if self.dirout :
            self.pathout = PathOut(dirout = self.dirout)
        if not self.parametres.get('isopen', False) :
            if self.tableau is None :
                self.tableau = parent.tableau
            self.tableau.parametres['mineff'] = 0
            dialcol = FreqDialog(self.parent, self.tableau.get_colnames(), _("Select columns"), size=(600, 250), showNA = False)
            dialcol.CenterOnParent()
            res = dialcol.ShowModal()
            if res == wx.ID_OK :
                if not self.actives :
                    self.tableau.selected_col = dialcol.m_listBox1.GetSelections()
                    actives = self.tableau.getactlistfromselection(self.tableau.selected_col)
                else :
                    actives = self.actives
                if isinstance(actives, dict) :
                    actives = [[val, actives[val][0]] for val in actives]
                    self.tableau.actives = dict(actives)
                self.tableau.make_listactives()
                actives = dict([[i, val] for i, val in enumerate(actives)])
                if not self.pathout :
                    self.parametres['pathout'] = ConstructPathOut(self.parametres['pathout'], 'Categorisation')
                else :
                    self.parametres['pathout'] = self.dirout
                self.pathout.createdir(self.parametres['pathout'])
                self.pathout.dirout = self.parametres['pathout']
                #self.doanalyse2()
            else :
                dialcol.Destroy()
                self.parametres = None
                return False

    def doanalyse(self) :
        count = 1
        keepGoing = self.dlg.Update(count)
        #----------------------------------------------------------------
        self.DictForme = {}
        self.Min = 10
        self.Linecontent = []
        self.tableau.dictpathout = self.pathout
        #self.DictPathOut['mat01'] = fromprof
        #self.PrintScript()
        cate = {'TOCATE':{}, 'CATE':{}}
        for val in self.tableau.actives :
            cate['TOCATE'][val] = self.tableau.actives[val][0]
        with open(self.pathout['cate.json'], 'w', encoding='utf8') as f :
            f.write(json.dumps(cate))
        self.tableau.save_tableau(self.pathout['analyse.db'])
        self.dlg.Destroy()

