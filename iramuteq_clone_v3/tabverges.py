# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os
import string
import sys
import tempfile
from time import sleep

#------------------------------------
# import des modules wx
#------------------------------------
import wx
import wx.lib.sized_controls as sc

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ffr,FFF, ConstructPathOut
from functions import exec_rcode, check_Rresult, progressbar
from PrintRScript import ProtoScript
from analysematrix import AnalyseMatrix
from dialog import ProtoDial


class Prototypical(AnalyseMatrix) :

#     def __init__(self, parent, parametres):
#         self.parent = parent
#         self.tableau = self.parent.tableau
#         self.parametres = parametres
#         self.parametres['filename'] = parent.tableau.parametre['filename']
#         self.parametres['pathout'] = ConstructPathOut(parent.tableau.parametre['filename'], 'proto')
#         self.parametres['type'] = 'proto'
#         dlg = progressbar(self.parent, 2)
#         self.colnames = self.tableau.get_colnames()
#         AnalyseMatrix.__init__(self, parent, parent.tableau, self.parametres, dlg = dlg)

    def doparametres(self, dlg = None):
        self.dial = ProtoDial(self.ira, self.tableau.colnames)
        self.dial.CenterOnParent()
        self.val = self.dial.ShowModal()
        if self.val==wx.ID_OK :
                self.ColSel1 = self.dial.variables.GetSelections()
                self.ColSel2 = self.dial.rangs.GetSelections()
                if len(self.ColSel1) != len(self.ColSel2) :
                    print('pas meme taille')
                    self.check_val()
                else :
                    if self.dial.choix_freq.GetSelection() == 0 :
                        self.parametres['limfreq'] = 'NULL'
                    else :
                        self.parametres['limfreq'] = self.dial.freqlim.GetValue()
                    if self.dial.choix_rang.GetSelection() == 0 :
                        self.parametres['limrang'] = 'NULL'
                    else :
                        self.parametres['limrang'] = self.dial.ranglim.GetValue()
                    self.parametres['freqmin'] = int(self.dial.m_textCtrl4.GetValue())
                    if self.dial.typegraph.GetSelection() == 0 :
                        self.parametres['typegraph'] = 'classical'
                        self.parametres['cloud'] = False
                    elif self.dial.typegraph.GetSelection() == 1 :
                        self.parametres['typegraph'] = 'classical'
                        self.parametres['cloud'] = True
                    else :
                        self.parametres['typegraph'] = 'plan'
                self.dial.Destroy()
        else :
            self.dial.Destroy()
            self.parametres = None

    def doanalyse(self) :
        table_assoc, table_rank = self.dotable()
        self.makedatas(table_assoc, table_rank)
        self.DoR()

    def dotable(self) :
        table_assoc = self.tableau.select_col(self.ColSel1)
        table_rank = self.tableau.select_col(self.ColSel2)
        return table_assoc, table_rank

    def makedatas(self, table_assoc, table_rank) :
        words = {}
        for i in range(0, len(table_assoc)) :
            for j, word in enumerate(table_assoc[i]) :
                if word.strip() != "" :
                    if word in words :
                        words[word][0] += 1
                        if table_rank[i][j] != '' :
                            words[word][1].append(float(table_rank[i][j]))
                    else :
                        if table_rank[i][j] != '' :
                            words[word] = [1, [float(table_rank[i][j])]]
                        else :
                            words[word] = [1, []]
        res = [[word, words[word][0], float(sum(words[word][1])) / len(words[word][1])] for word in words if len(words[word][1]) != 0 and words[word][0] >= self.parametres['freqmin']]
        with open(self.pathout['table.csv'], 'w', encoding='utf8') as f :
            f.write('\n'.join(['\t'.join(['"' + val[0] +'"', repr(val[1]), repr(val[2])]) for val in res]))
        #self.parent.tableau.parametres = self.parent.tableau.parametre
        #self.parent.tableau.save_tableau(self.pathout['analyse.db'])

    def DoR(self) :
        script = ProtoScript(self)
        script.make_script()
        self.doR(script.scriptout) 
