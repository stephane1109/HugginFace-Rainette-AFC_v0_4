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
import logging

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ConstructPathOut, StatTxtPathOut, PathOut, ffr
from analysetxt import AnalyseText
from functions import exec_rcode, progressbar, check_Rresult, CreateIraFile, print_liste, treat_var_mod, write_tab, DoConf, TGen
from dialog import OptLexi
from PrintRScript import LabbeScript


log = logging.getLogger('iramuteq.labbe')


class DistLabbe(AnalyseText) :

    def doanalyse(self) :
        pathout = self.pathout.dirout
        self.dictpathout = StatTxtPathOut(pathout)
        self.parametres['ira'] = self.dictpathout['ira']
        self.make_lexico()
        if self.dlg :
            try :
                self.dlg.Destroy()
            except :
                pass

    def preferences(self) :
        listet = self.corpus.make_etoiles()
        listet.sort()
        variables = treat_var_mod(listet)
        var = [v for v in variables]
        dial = OptLexi(self.parent)
        dial.listet = listet
        dial.variables = var
        for et in var :
            dial.list_box_1.Append(et)
        dial.CenterOnParent()
        self.dialok = dial.ShowModal()
        if self.dialok == wx.ID_OK :
            if dial.choice.GetSelection() == 1 :
                ListEt = [listet[i] for i in dial.list_box_1.GetSelections()]
            else :
                ListEt = variables[var[dial.list_box_1.GetSelections()[0]]]
            self.listet = ListEt
            self.listet.sort()
            self.parametres['mineff'] = dial.spin.GetValue()
            if dial.choice_indice.GetSelection() == 0 :
                self.parametres['indice'] = 'hypergeo'
            else :
                self.parametres['indice'] = 'chi2'
            self.parametres['typeformes'] = dial.typeformes.GetSelection()
            self.parametres['clnb'] = len(ListEt)
            dial.Destroy()
            return self.parametres
        else :
            dial.Destroy()
            return None

    def make_lexico(self) :
        mineff = self.parametres['mineff']
        tabout = self.corpus.make_lexitable(mineff, self.listet, gram = self.parametres['typeformes'])
        write_tab(tabout, self.dictpathout['tableafcm'])
        #tabout = self.corpus.make_efftype_from_etoiles(self.listet)
        #write_tab(tabout, self.dictpathout['tabletypem'])
        self.dlg = progressbar(self, 3)
        if self.dlg :
            self.dlg.Update(2, 'R...')
        script = LabbeScript(self)
        script.make_script()
        self.doR(script.scriptout, dlg = self.dlg, message = 'R...')
        if self.dlg :
            self.dlg.Update(3, 'Chargement...')
        afcf_graph_list = [[os.path.basename(self.dictpathout['afcf_row']), 'lignes'],\
                            [os.path.basename(self.dictpathout['afcf_col']), 'colonnes']]
        #afct_graph_list = [[os.path.basename(self.dictpathout['afct_row']), 'lignes'],\
        #                    [os.path.basename(self.dictpathout['afct_col']), 'colonnes']]
        print_liste(self.dictpathout['liste_graph_afcf'],afcf_graph_list)
        #print_liste(self.dictpathout['liste_graph_afct'],afct_graph_list)
        self.dlg.Destroy()
