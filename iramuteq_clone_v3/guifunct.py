# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os
import sys
from copy import copy
import codecs


#------------------------------------
# import des modules wx
#------------------------------------
import wx
import wx.lib.agw.hyperlink as hl

#------------------------------------
# import des fichiers du projet
#------------------------------------
from dialog import FileOptionDialog, SelectColDial, OptLexi, PrefSimpleFile
from listlex import *
from vitemspicker import VItemsPicker, EVT_IP_SELECTION_CHANGED, IP_SORT_CHOICES, IP_SORT_SELECTED, IP_REMOVE_FROM_CHOICES
from functions import treat_var_mod, print_liste, exec_rcode, check_Rresult, DoConf, read_list_file, indices_simi
from webexport import WebExport
from PrintRScript import PrintSimiScript

import langue
langue.run()


def OnOpen(self, type):
        if type == "Data":
            wildcard = "Fichiers supportés|*.ods;*.xls;*.csv;*.txt|Openoffice Calc|*.ods|Excel 97/2000/XP/2003|*.xls|Fichier csv|*.csv|Fichier texte|*.txt|Tous les fichiers|*"
        elif type == "Texte":
            wildcard = "Fichier texte|*.txt|Tous les fichiers|*"
        elif type == "Analyse":
            wildcard = "Fichier analyse/Corpus|*.ira;*.cira"
        defaultDir = self.PathPath.get('PATHS', 'lastpath')
        if defaultDir.strip() == '':
            defaultDir = self.UserConfigPath.replace('.iramuteq','')
        dlg = wx.FileDialog(
        self, message=_("Choose a file"), defaultDir=defaultDir,
        defaultFile="", wildcard=wildcard, style=wx.FD_OPEN | wx.FD_CHANGE_DIR)
        dlg.CenterOnParent()
        if dlg.ShowModal() == wx.ID_OK :
            fileName = dlg.GetFilename()
            path = dlg.GetPaths()
            dlg.Destroy()
            self.PathPath.set('PATHS', 'lastpath', os.path.dirname(path[0]))
            self.type = type
            return fileName, path
        else:
            dlg.Destroy()
            if type == "Data":
                return False, [False]
            elif type == "Texte":
                return False, [False]
            elif type == "Analyse":
                return False

def getfileextension(file) :
    return os.path.splitext(file)[1]

def get_table_param(self, filename) :
    if getfileextension(filename) == '.csv':
        dlg = FileOptionDialog(self, -1, _("File format"), sep=True, size=(350, 200),
                     style=wx.DEFAULT_DIALOG_STYLE)
        dlg.CenterOnParent()
        val = dlg.ShowModal()
        if val == wx.ID_OK:
            self.tableau.parametres['colsep'] = dlg.colsep[dlg.choice3.GetSelection()]
            self.tableau.parametres['txtsep'] = dlg.txtsep[dlg.choice4.GetSelection()]
            if self.tableau.parametres['colsep'] == 'tabulation' :
                self.tableau.parametres['colsep'] = '\t'
            self.tableau.parametres['filetype'] = 'csv'
            self.tableau.parametres['encodage'] = dlg.le[dlg.list_encodages.GetSelection()]
    elif  getfileextension(filename) == '.xls' :
        dlg = FileOptionDialog(self, -1, _("File format"), sep=False, sheet = True, size=(350, 200),
                     style=wx.DEFAULT_DIALOG_STYLE)
        dlg.CenterOnParent()
        val = dlg.ShowModal()
        if val == wx.ID_OK:
            self.tableau.parametres['colsep'] = ';'
            self.tableau.parametres['txtsep'] = '\"'
            self.tableau.parametres['encodage'] = sys.getdefaultencoding()
            self.tableau.parametres['sheetnb'] = dlg.spin1.GetValue()
            self.tableau.parametres['filetype'] = 'xls'
    elif getfileextension(filename) == '.ods':
        dlg = FileOptionDialog(self, -1, _("File format"), sep=False, size=(350, 200),
                     style=wx.DEFAULT_DIALOG_STYLE)
        dlg.CenterOnParent()
        val = dlg.ShowModal()
        if val == wx.ID_OK:
            self.tableau.parametres['colsep'] = ';'
            self.tableau.parametres['txtsep'] = '\"'
            self.tableau.parametres['filetype'] = 'ods'
    else :
        val = False
    if val == wx.ID_OK:
        if dlg.radio_box_1.GetSelection() == 0:
            self.tableau.firstrowiscolnames = True
        else:
            self.tableau.firstrowiscolnames = False
        if dlg.radio_box_2.GetSelection() == 0:
            self.tableau.firstcolisrownames = True
        else:
            self.tableau.firstcolisrownames = False
    dlg.Destroy()
    return val

def getPage(ira) :
    if '_mgr' in dir(ira) :
        if not ira._mgr.GetPane('Text').IsShown() :
            if ira.nb.GetPageCount() >= 1:
                return ira.nb.GetPage(ira.nb.GetSelection())
            else :
                return None
        else :
            return None
    else :
        return None

def getCorpus(page) :
    if 'corpus' in page.__dict__:
        return copy(page.corpus)
    else :
        return None

class SelectColumn :
    def __init__(self, parent, dictcol, actives, pathout, selected = None, dlg = False) :
        self.ira = parent
        if dlg :
            dial = SelectColDial(self.ira)
            listcol = ListForSpec(dial, self, dictcol, ['eff'])
            dial.bSizer2.Add( listcol, 2, wx.ALL|wx.EXPAND, 5 )
            dial.m_sdbSizer2.AddButton( dial.m_sdbSizer2OK )
            dial.m_sdbSizer2.AddButton( dial.butok)
            dial.m_sdbSizer2.Realize()
            dial.bSizer2.Add( dial.m_sdbSizer2, 0, wx.EXPAND, 5 )
            dial.Layout()
            if selected is None :
                for row in range(listcol.GetItemCount()):
                    listcol.Select(row)
            else :
                orderlex = dict([[listcol.getColumnText(i,0),i] for i in range(0,listcol.GetItemCount())])
                for row in selected :
                    listcol.Select(orderlex[actives[row]])
            dial.CenterOnParent()
            val = dial.ShowModal()
            if val == wx.ID_OK :
                last = listcol.GetFirstSelected()
                lastl = [listcol.GetFirstSelected()]
                indexes = [listcol.getColumnText(listcol.GetFirstSelected(),0)]
                while listcol.GetNextSelected(last) != -1:
                    last = listcol.GetNextSelected(last)
                    lastl.append(last)
                    indexes.append(listcol.getColumnText(last,0))
                dial.Destroy()
                column = [actives.index(val) for val in indexes]
                column.sort()
                with open(pathout, 'w' ,encoding='utf8') as f :
                    f.write('\n'.join([repr(val) for val in column]))
                self.ok = True
            else :
                self.ok = False
        else :
            self.ok = True
            if selected is None :
                selected = [i for i in range(0, len(actives))]
            with open(pathout, 'w', encoding='utf8') as f :
                f.write('\n'.join([repr(i) for i in selected]))


class PrefSimi ( wx.Dialog ):

    def __init__( self, parent, ID, paramsimi, indices, wordlist = None, selected = None, actives = None):
        wx.Dialog.__init__ ( self, None, id = wx.ID_ANY, title = _("Settings"), pos = wx.DefaultPosition, size = wx.Size( -1,-1 ), style = wx.DEFAULT_DIALOG_STYLE )
        self.parent = parent
        self.ira = parent
        self.paramsimi=paramsimi
        self.indices = indices

        self.SetSizeHints( wx.DefaultSize, wx.DefaultSize )

        bSizer16 = wx.BoxSizer( wx.HORIZONTAL )
        if wordlist is not None :
            self.listcol = ListForSpec(self, self, wordlist, ['eff'])
            self.listcol.SetMinSize( wx.Size( 270,-1 ) )
            listsizer = wx.BoxSizer( wx.VERTICAL )
            countsizer = wx.BoxSizer( wx.HORIZONTAL )
            self.butcount = wx.Button(self, -1, _("count"))
            self.textcount = wx.TextCtrl(self, -1, "", wx.DefaultPosition, wx.Size( 100, 50 ), wx.TE_READONLY )
            countsizer.Add(self.butcount, 0, wx.ALL, 5)
            countsizer.Add(self.textcount, 0, wx.ALL, 5 )
            listsizer.Add(countsizer, 0, wx.ALL, 5)
            listsizer.Add(self.listcol, 2, wx.ALL|wx.EXPAND, 5 )
            #bSizer16.Add( self.listcol, 0, wx.ALL|wx.EXPAND, 5 )
            bSizer16.Add( listsizer, 1, wx.ALL|wx.EXPAND, 5)
            if selected is None :
                maxsel = self.listcol.GetItemCount()
                if maxsel > 200 :
                    maxsel = 200
                for row in range(maxsel):
                    self.listcol.Select(row)
            else :
                self.orderlex = dict([[self.listcol.getColumnText(i,0),i] for i in range(0,self.listcol.GetItemCount())])
                for row in selected :
                    self.listcol.Select(self.orderlex[actives[row]])


        fgSizer10 = wx.FlexGridSizer( 2, 1, 0, 0 )
        fgSizer10.SetFlexibleDirection( wx.BOTH )
        fgSizer10.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )

        self.m_notebook1 = wx.Notebook( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_panel2 = wx.Panel( self.m_notebook1, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
        bSizer18 = wx.BoxSizer( wx.VERTICAL )

        fgSizer3 = wx.FlexGridSizer( 0, 2, 0, 0 )
        fgSizer3.SetFlexibleDirection( wx.BOTH )
        fgSizer3.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )

        if not self.paramsimi['first'] :

            self.m_staticText271 = wx.StaticText( self.m_panel2, wx.ID_ANY, _("Use previous coordinates"), wx.DefaultPosition, wx.DefaultSize, 0 )
            self.m_staticText271.Wrap( -1 )
            fgSizer3.Add( self.m_staticText271, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

            self.check_coord = wx.CheckBox( self.m_panel2, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
            fgSizer3.Add( self.check_coord, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

            self.m_staticline36 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
            fgSizer3.Add( self.m_staticline36, 0, wx.EXPAND, 5 )

            self.m_staticline37 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
            fgSizer3.Add( self.m_staticline37, 0, wx.EXPAND, 5 )

        self.m_staticText3 = wx.StaticText( self.m_panel2, wx.ID_ANY, _("Score"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText3.Wrap( -1 )
        fgSizer3.Add( self.m_staticText3, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        choice1Choices = []
        self.choice1 = wx.Choice( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, self.indices, 0 )
        self.choice1.SetSelection( 0 )
        fgSizer3.Add( self.choice1, 0, wx.ALL, 5 )

        self.m_staticline293 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline293, 0, wx.EXPAND, 5 )

        self.m_staticline292 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline292, 0, wx.EXPAND, 5 )

        self.m_staticText4 = wx.StaticText( self.m_panel2, wx.ID_ANY, _("Layout"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText4.Wrap( -1 )
        fgSizer3.Add( self.m_staticText4, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        choice2Choices = [ "random", "cercle", "fruchterman reingold", "kamada kawai", "graphopt" ]
        if 'word' in self.paramsimi :
            choice2Choices += ["spirale", 'spirale3D']
        self.choice2 = wx.Choice( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, choice2Choices, 0 )
        self.choice2.SetSelection( 0 )
        fgSizer3.Add( self.choice2, 0, wx.ALL, 5 )

        self.m_staticline294 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline294, 0, wx.EXPAND, 5 )

        self.m_staticline295 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline295, 0, wx.EXPAND, 5 )

        self.m_staticText5 = wx.StaticText( self.m_panel2, wx.ID_ANY, _("Graphic type"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText5.Wrap( -1 )
        fgSizer3.Add( self.m_staticText5, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        choice3Choices = [ "dynamique", "statique", "3D"]#, 'web2D', "web3D" ]
        self.choice3 = wx.Choice( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, choice3Choices, 0 )
        self.choice3.SetSelection( 0 )

        label_format = wx.StaticText(self.m_panel2, -1, _("Picture format"))
        self.choix_format =  wx.Choice(self.m_panel2, -1, (100,50), choices = ['png', 'svg'])
        self.choix_format.SetSelection( 0 )
        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        hsizer.Add(self.choice3, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )
        hsizer.Add(label_format, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )
        hsizer.Add(self.choix_format, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )
        fgSizer3.Add( hsizer, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.m_staticline296 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline296, 0, wx.EXPAND, 5 )

        self.m_staticline297 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline297, 0, wx.EXPAND, 5 )

        self.m_staticText8 = wx.StaticText( self.m_panel2, wx.ID_ANY, _("Maximum tree"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText8.Wrap( -1 )
        fgSizer3.Add( self.m_staticText8, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.check1 = wx.CheckBox( self.m_panel2, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
        fgSizer3.Add( self.check1, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.m_staticline298 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline298, 0, wx.EXPAND, 5 )

        self.m_staticline299 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline299, 0, wx.EXPAND, 5 )

        self.m_staticText91 = wx.StaticText( self.m_panel2, wx.ID_ANY, _("Edges threshold"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText91.Wrap( -1 )
        fgSizer3.Add( self.m_staticText91, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        bSizer21 = wx.BoxSizer( wx.HORIZONTAL )

        self.check_seuil = wx.CheckBox( self.m_panel2, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
        bSizer21.Add( self.check_seuil, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.spin_seuil = wx.SpinCtrl( self.m_panel2, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 1, 10000, 1 )
        bSizer21.Add( self.spin_seuil, 0, wx.ALL, 5 )


        fgSizer3.Add( bSizer21, 1, wx.EXPAND, 5 )

        self.m_staticline2910 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline2910, 0, wx.EXPAND, 5 )

        self.m_staticline2911 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline2911, 0, wx.EXPAND, 5 )

        self.m_staticText19 = wx.StaticText( self.m_panel2, wx.ID_ANY, _("Text on vertex"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText19.Wrap( -1 )
        fgSizer3.Add( self.m_staticText19, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.check_vlab = wx.CheckBox( self.m_panel2, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
        fgSizer3.Add( self.check_vlab, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.m_staticline2912 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline2912, 0, wx.EXPAND, 5 )

        self.m_staticline2913 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline2913, 0, wx.EXPAND, 5 )

        self.m_staticText20 = wx.StaticText( self.m_panel2, wx.ID_ANY, _("Score on edges"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText20.Wrap( -1 )
        fgSizer3.Add( self.m_staticText20, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.check_elab = wx.CheckBox( self.m_panel2, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
        fgSizer3.Add( self.check_elab, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.m_staticline39 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline39, 0, wx.EXPAND |wx.ALL, 5 )

        self.m_staticline40 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline40, 0, wx.EXPAND |wx.ALL, 5 )

        self.m_staticText321 = wx.StaticText( self.m_panel2, wx.ID_ANY, _("Edge curved"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText321.Wrap( -1 )
        fgSizer3.Add( self.m_staticText321, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.check_curved = wx.CheckBox( self.m_panel2, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
        fgSizer3.Add( self.check_curved, 0, wx.ALL, 5 )

        self.m_staticline2914 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline2914, 0, wx.EXPAND, 5 )

        self.m_staticline2915 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline2915, 0, wx.EXPAND, 5 )

        self.m_staticText27 = wx.StaticText( self.m_panel2, wx.ID_ANY, _("Text size"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText27.Wrap( -1 )
        fgSizer3.Add( self.m_staticText27, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.spin_cex = wx.SpinCtrl( self.m_panel2, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 0, 100, 10 )
        fgSizer3.Add( self.spin_cex, 0, wx.ALL, 5 )

        self.m_staticline2916 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline2916, 0, wx.EXPAND, 5 )

        self.m_staticline2917 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer3.Add( self.m_staticline2917, 0, wx.EXPAND, 5 )

        bsizer34 = wx.BoxSizer(wx.HORIZONTAL)

        comtext =  wx.StaticText( self.m_panel2, wx.ID_ANY, _("Communities"), wx.DefaultPosition, wx.DefaultSize, 0 )
        comtext.Wrap( -1 )
        bsizer34.Add(comtext, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.comcheck = wx.CheckBox(self.m_panel2, -1)
        bsizer34.Add(self.comcheck, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        fgSizer3.Add(bsizer34 , 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        sizer54 = wx.BoxSizer(wx.HORIZONTAL)
        self.comlist = ['edge.betweenness.community','fastgreedy.community','label.propagation.community','leading.eigenvector.community','multilevel.community','optimal.community', 'spinglass.community', 'walktrap.community']
        self.choix_com = wx.Choice( self.m_panel2, wx.ID_ANY, choices = self.comlist)
        self.choix_com.SetSelection( 0 )
        self.halo = wx.CheckBox(self.m_panel2, wx.ID_ANY, 'halo')
        sizer54.Add(self.choix_com , 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )
        sizer54.Add(self.halo , 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )
        fgSizer3.Add( sizer54, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        if 'bystar' in self.paramsimi :
            self.m_staticText40 = wx.StaticText( self.m_panel2, wx.ID_ANY, _("Select a variable"), wx.DefaultPosition, wx.DefaultSize, 0 )
            self.m_staticText40.Wrap( -1 )
            fgSizer3.Add( self.m_staticText40, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

            self.check_bystar = wx.CheckBox( self.m_panel2, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
            fgSizer3.Add( self.check_bystar, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

            self.m_staticline3200 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
            fgSizer3.Add( self.m_staticline3200, 0, wx.EXPAND, 5 )
            self.m_staticline3201 = wx.StaticLine( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
            fgSizer3.Add( self.m_staticline3201, 0, wx.EXPAND, 5 )


        bSizer18.Add( fgSizer3, 0, wx.EXPAND, 5 )


        self.m_panel2.SetSizer( bSizer18 )
        self.m_panel2.Layout()
        bSizer18.Fit( self.m_panel2 )
        self.m_notebook1.AddPage( self.m_panel2, _("Graph settings"), True )
        self.m_panel3 = wx.Panel( self.m_notebook1, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
        fgSizer5 = wx.FlexGridSizer( 0, 3, 0, 0 )
        fgSizer5.SetFlexibleDirection( wx.BOTH )
        fgSizer5.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )

        fgSizer51 = wx.FlexGridSizer( 0, 2, 0, 0 )
        fgSizer51.SetFlexibleDirection( wx.BOTH )
        fgSizer51.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )

        self.m_staticText6 = wx.StaticText( self.m_panel3, wx.ID_ANY, _("Picture size"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText6.Wrap( -1 )
        fgSizer51.Add( self.m_staticText6, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        fgSizer31 = wx.FlexGridSizer( 0, 2, 0, 0 )
        fgSizer31.SetFlexibleDirection( wx.BOTH )
        fgSizer31.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )

        self.m_staticText9 = wx.StaticText( self.m_panel3, wx.ID_ANY, _("height"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText9.Wrap( -1 )
        fgSizer31.Add( self.m_staticText9, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.spin_height = wx.SpinCtrl( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 10, 100000, 800 )
        fgSizer31.Add( self.spin_height, 0, wx.ALL, 5 )

        self.m_staticText10 = wx.StaticText( self.m_panel3, wx.ID_ANY, _("width"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText10.Wrap( -1 )
        fgSizer31.Add( self.m_staticText10, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.spin_width = wx.SpinCtrl( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 10, 100000, 800 )
        fgSizer31.Add( self.spin_width, 0, wx.ALL, 5 )


        fgSizer51.Add( fgSizer31, 1, wx.EXPAND, 5 )

        self.m_staticline3 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline3, 0, wx.EXPAND, 5 )

        self.m_staticline4 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline4, 0, wx.EXPAND, 5 )

        self.m_staticText101 = wx.StaticText( self.m_panel3, wx.ID_ANY, _("Vertex size proportional to frequency"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText101.Wrap( -1 )
        fgSizer51.Add( self.m_staticText101, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        bSizer7 = wx.BoxSizer( wx.HORIZONTAL )

        bSizer9 = wx.BoxSizer( wx.VERTICAL )

        self.check2 = wx.CheckBox( self.m_panel3, wx.ID_ANY, "eff.", wx.DefaultPosition, wx.DefaultSize, 0 )
        bSizer9.Add( self.check2, 0, wx.ALL, 5 )

        self.checki = wx.CheckBox( self.m_panel3, wx.ID_ANY, "chi2", wx.DefaultPosition, wx.DefaultSize, 0 )
        bSizer9.Add( self.checki, 0, wx.ALL, 5 )


        bSizer7.Add( bSizer9, 0, wx.ALIGN_CENTER_VERTICAL, 5 )

        fgSizer7 = wx.FlexGridSizer( 0, 2, 0, 0 )
        fgSizer7.SetFlexibleDirection( wx.BOTH )
        fgSizer7.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )

        self.m_staticText11 = wx.StaticText( self.m_panel3, wx.ID_ANY, "min", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText11.Wrap( -1 )
        fgSizer7.Add( self.m_staticText11, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.spin_tvmin = wx.SpinCtrl( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 0, 100, 0 )
        fgSizer7.Add( self.spin_tvmin, 0, wx.ALL, 5 )

        self.m_staticText12 = wx.StaticText( self.m_panel3, wx.ID_ANY, "max", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText12.Wrap( -1 )
        fgSizer7.Add( self.m_staticText12, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.spin_tvmax = wx.SpinCtrl( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 0, 100, 0 )
        fgSizer7.Add( self.spin_tvmax, 0, wx.ALL, 5 )


        bSizer7.Add( fgSizer7, 1, wx.EXPAND, 5 )


        fgSizer51.Add( bSizer7, 1, wx.EXPAND, 5 )

        self.m_staticline31 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline31, 0, wx.EXPAND, 5 )

        self.m_staticline32 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline32, 0, wx.EXPAND, 5 )

        self.m_staticText1011 = wx.StaticText( self.m_panel3, wx.ID_ANY, _("Vertex text size proportional to frequency"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText1011.Wrap( -1 )
        fgSizer51.Add( self.m_staticText1011, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        bSizer71 = wx.BoxSizer( wx.HORIZONTAL )

        bSizer8 = wx.BoxSizer( wx.VERTICAL )

        self.check_vcex = wx.CheckBox( self.m_panel3, wx.ID_ANY, "eff.", wx.DefaultPosition, wx.DefaultSize, 0 )
        bSizer8.Add( self.check_vcex, 0, wx.ALL, 5 )

        self.checkit = wx.CheckBox( self.m_panel3, wx.ID_ANY, "chi2", wx.DefaultPosition, wx.DefaultSize, 0 )
        bSizer8.Add( self.checkit, 0, wx.ALL, 5 )


        bSizer71.Add( bSizer8, 0, wx.ALIGN_CENTER_VERTICAL, 5 )

        fgSizer71 = wx.FlexGridSizer( 0, 2, 0, 0 )
        fgSizer71.SetFlexibleDirection( wx.BOTH )
        fgSizer71.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )

        self.m_staticText111 = wx.StaticText( self.m_panel3, wx.ID_ANY, "min", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText111.Wrap( -1 )
        fgSizer71.Add( self.m_staticText111, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.spin_vcexmin = wx.SpinCtrl( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 0, 100, 0 )
        fgSizer71.Add( self.spin_vcexmin, 0, wx.ALL, 5 )

        self.m_staticText121 = wx.StaticText( self.m_panel3, wx.ID_ANY, "max", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText121.Wrap( -1 )
        fgSizer71.Add( self.m_staticText121, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.spin_vcexmax = wx.SpinCtrl( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 0, 100, 0 )
        fgSizer71.Add( self.spin_vcexmax, 0, wx.ALL, 5 )


        bSizer71.Add( fgSizer71, 1, wx.EXPAND, 5 )


        fgSizer51.Add( bSizer71, 1, wx.EXPAND, 5 )

        self.m_staticline321 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline321, 0, wx.EXPAND, 5 )

        self.m_staticline322 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline322, 0, wx.EXPAND, 5 )

        self.m_staticText10111 = wx.StaticText( self.m_panel3, wx.ID_ANY, _("Edges width proportional to score"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText10111.Wrap( -1 )
        fgSizer51.Add( self.m_staticText10111, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        bSizer711 = wx.BoxSizer( wx.HORIZONTAL )

        self.check3 = wx.CheckBox( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
        bSizer711.Add( self.check3, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        fgSizer711 = wx.FlexGridSizer( 0, 2, 0, 0 )
        fgSizer711.SetFlexibleDirection( wx.BOTH )
        fgSizer711.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )

        self.m_staticText1111 = wx.StaticText( self.m_panel3, wx.ID_ANY, "min", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText1111.Wrap( -1 )
        fgSizer711.Add( self.m_staticText1111, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.spin_temin = wx.SpinCtrl( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 0, 100, 0 )
        fgSizer711.Add( self.spin_temin, 0, wx.ALL, 5 )

        self.m_staticText1211 = wx.StaticText( self.m_panel3, wx.ID_ANY, "max", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText1211.Wrap( -1 )
        fgSizer711.Add( self.m_staticText1211, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.spin_temax = wx.SpinCtrl( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 0, 100, 0 )
        fgSizer711.Add( self.spin_temax, 0, wx.ALL, 5 )


        bSizer711.Add( fgSizer711, 1, wx.EXPAND, 5 )


        fgSizer51.Add( bSizer711, 1, wx.EXPAND, 5 )

        self.m_staticline33 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline33, 0, wx.EXPAND, 5 )

        self.m_staticline34 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline34, 0, wx.EXPAND, 5 )

        self.m_staticText28 = wx.StaticText( self.m_panel3, wx.ID_ANY, _("Gray scale on text proportional to frequency (0=black, 1=white)"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText28.Wrap( -1 )
        fgSizer51.Add( self.m_staticText28, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        bSizer10 = wx.BoxSizer( wx.HORIZONTAL )

        self.m_checkBox14 = wx.CheckBox( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
        bSizer10.Add( self.m_checkBox14, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        bSizer11 = wx.BoxSizer( wx.VERTICAL )

        bSizer12 = wx.BoxSizer( wx.HORIZONTAL )

        self.m_staticText31 = wx.StaticText( self.m_panel3, wx.ID_ANY, "min", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText31.Wrap( -1 )
        bSizer12.Add( self.m_staticText31, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.m_spinCtrl14 = wx.SpinCtrl( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 0, 100, 0 )
        bSizer12.Add( self.m_spinCtrl14, 0, wx.ALL, 5 )


        bSizer11.Add( bSizer12, 1, wx.EXPAND, 5 )

        bSizer13 = wx.BoxSizer( wx.HORIZONTAL )

        self.m_staticText32 = wx.StaticText( self.m_panel3, wx.ID_ANY, "max", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText32.Wrap( -1 )
        bSizer13.Add( self.m_staticText32, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.m_spinCtrl15 = wx.SpinCtrl( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 0, 100, 10 )
        bSizer13.Add( self.m_spinCtrl15, 0, wx.ALL, 5 )


        bSizer11.Add( bSizer13, 1, wx.EXPAND, 5 )


        bSizer10.Add( bSizer11, 1, wx.EXPAND, 5 )


        fgSizer51.Add( bSizer10, 1, wx.ALIGN_CENTER_VERTICAL|wx.EXPAND, 5 )

        self.m_staticline3311 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline3311, 0, wx.EXPAND |wx.ALL, 5 )

        self.m_staticline33111 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline33111, 0, wx.EXPAND |wx.ALL, 5 )

        bSizer5 = wx.BoxSizer( wx.HORIZONTAL )

        self.m_staticText21 = wx.StaticText( self.m_panel3, wx.ID_ANY, _("Vertex color"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText21.Wrap( -1 )
        bSizer5.Add( self.m_staticText21, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.cols = wx.ColourPickerCtrl( self.m_panel3, wx.ID_ANY, wx.Colour( 255, 0, 0 ), wx.DefaultPosition, wx.DefaultSize, wx.CLRP_DEFAULT_STYLE )
        bSizer5.Add( self.cols, 0, wx.ALL, 5 )


        fgSizer51.Add( bSizer5, 1, wx.EXPAND, 5 )

        bSizer6 = wx.BoxSizer( wx.HORIZONTAL )

        self.m_staticText22 = wx.StaticText( self.m_panel3, wx.ID_ANY, _("Edges color"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText22.Wrap( -1 )
        bSizer6.Add( self.m_staticText22, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.cola = wx.ColourPickerCtrl( self.m_panel3, wx.ID_ANY, wx.Colour( 208, 208, 208 ), wx.DefaultPosition, wx.DefaultSize, wx.CLRP_DEFAULT_STYLE )
        bSizer6.Add( self.cola, 0, wx.ALL, 5 )


        fgSizer51.Add( bSizer6, 1, wx.EXPAND, 5 )

        self.m_staticline331 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline331, 0, wx.EXPAND, 5 )

        self.m_staticline332 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline332, 0, wx.EXPAND, 5 )

        self.m_staticText23 = wx.StaticText( self.m_panel3, wx.ID_ANY, _("Vertex size"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText23.Wrap( -1 )
        fgSizer51.Add( self.m_staticText23, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        bSizer72 = wx.BoxSizer( wx.HORIZONTAL )

        self.check_s_size = wx.CheckBox( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
        bSizer72.Add( self.check_s_size, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.spin_tv = wx.SpinCtrl( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.SP_ARROW_KEYS, 0, 100, 10 )
        bSizer72.Add( self.spin_tv, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )


        fgSizer51.Add( bSizer72, 1, wx.EXPAND, 5 )

        self.m_staticline333 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline333, 0, wx.EXPAND, 5 )

        self.m_staticline334 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline334, 0, wx.EXPAND, 5 )

        self.m_staticText24 = wx.StaticText( self.m_panel3, wx.ID_ANY, _("Spheres transparency"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText24.Wrap( -1 )
        fgSizer51.Add( self.m_staticText24, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.slider_sphere = wx.Slider( self.m_panel3, wx.ID_ANY, 10, 0, 100, wx.DefaultPosition, wx.DefaultSize, wx.SL_HORIZONTAL|wx.SL_LABELS )
        fgSizer51.Add( self.slider_sphere, 0, wx.ALL|wx.EXPAND|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.m_staticline335 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline335, 0, wx.EXPAND, 5 )

        self.m_staticline336 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline336, 0, wx.EXPAND, 5 )

        self.m_staticText25 = wx.StaticText( self.m_panel3, wx.ID_ANY, _("Make a movie"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText25.Wrap( -1 )
        fgSizer51.Add( self.m_staticText25, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.film = wx.CheckBox( self.m_panel3, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
        fgSizer51.Add( self.film, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5 )

        self.m_staticline2918 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline2918, 0, wx.EXPAND, 5 )

        self.m_staticline2919 = wx.StaticLine( self.m_panel3, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
        fgSizer51.Add( self.m_staticline2919, 0, wx.EXPAND, 5 )


        fgSizer51.AddStretchSpacer(wx.EXPAND)


        fgSizer5.Add( fgSizer51, 1, wx.EXPAND, 5 )


        self.m_panel3.SetSizer( fgSizer5 )
        self.m_panel3.Layout()
        fgSizer5.Fit( self.m_panel3 )
        self.m_notebook1.AddPage( self.m_panel3, _("Graphical settings"), False )

        fgSizer10.Add( self.m_notebook1, 1, wx.EXPAND |wx.ALL, 5 )

        m_sdbSizer2 = wx.StdDialogButtonSizer()
        self.m_sdbSizer2OK = wx.Button( self, wx.ID_OK )
        m_sdbSizer2.AddButton( self.m_sdbSizer2OK )
        self.m_sdbSizer2Cancel = wx.Button( self, wx.ID_CANCEL )
        m_sdbSizer2.AddButton( self.m_sdbSizer2Cancel )
        m_sdbSizer2.Realize();

        fgSizer10.Add( m_sdbSizer2, 1, wx.EXPAND, 5 )


        bSizer16.Add( fgSizer10, 3, wx.EXPAND, 5 )


        self.SetSizer( bSizer16 )
        self.Layout()
        bSizer16.Fit( self )

        self.Centre( wx.BOTH )
        self.__set_properties()

        # Connect Events
        if not self.paramsimi['first'] :
            self.check_coord.Bind( wx.EVT_CHECKBOX, self.OnKeepCoords )
        self.choice3.Bind( wx.EVT_CHOICE, self.OnChangeType )
        self.check2.Bind( wx.EVT_CHECKBOX, self.OnCheck2 )
        if 'cexfromchi' in self.paramsimi :
            self.checkit.Bind( wx.EVT_CHECKBOX, self.OnCheckit )
        if 'sfromchi' in self.paramsimi :
            self.checki.Bind( wx.EVT_CHECKBOX, self.OnChecki )
        self.check_vcex.Bind( wx.EVT_CHECKBOX, self.OnCheck_vcex )
        self.check_s_size.Bind( wx.EVT_CHECKBOX, self.OnCheck_s_size )
        self.listcol.Bind( wx.EVT_LIST_ITEM_SELECTED, self.ChangeCount)
        self.listcol.Bind( wx.EVT_LIST_ITEM_DESELECTED, self.ChangeCount)
        self.butcount.Bind( wx.EVT_BUTTON, self.ChangeCount)
        self.ChangeCount(wx.EVT_BUTTON)


    def __set_properties(self):
        self.choice1.SetSelection(self.paramsimi['coeff'])
        self.choice2.SetSelection(self.paramsimi['layout'])
        self.choice3.SetSelection(self.paramsimi['type_graph'])
        if self.paramsimi['type_graph'] not in  [2,3] :
            self.film.Enable(False)
            self.slider_sphere.Enable(False)
        else :
            self.film.Enable(True)
            self.slider_sphere.Enable(True)
        self.check1.SetValue(self.paramsimi['arbremax'])
        self.check_vlab.SetValue(self.paramsimi['label_v'])
        self.check_elab.SetValue(self.paramsimi['label_e'])
        self.check2.SetValue(self.paramsimi['tvprop'])
        self.spin_tv.SetValue(self.paramsimi['coeff_tv_nb'])
        self.check_s_size.SetValue(self.paramsimi['coeff_tv'])
        self.spin_tvmin.SetValue(self.paramsimi['tvmin'])
        self.spin_tvmax.SetValue(self.paramsimi['tvmax'])
        self.check3.SetValue(self.paramsimi['coeff_te'])
        self.spin_temin.SetValue(self.paramsimi['coeff_temin'])
        self.spin_temax.SetValue(self.paramsimi['coeff_temax'])
        self.check_vcex.SetValue(self.paramsimi['vcex'])
        self.spin_vcexmin.SetValue(self.paramsimi['vcexmin'])
        self.spin_vcexmax.SetValue(self.paramsimi['vcexmax'])
        self.spin_cex.SetValue(self.paramsimi['cex'])
        self.check_seuil.SetValue(self.paramsimi['seuil_ok'])
        self.spin_seuil.SetValue(self.paramsimi['seuil'])
        self.cols.SetColour(self.paramsimi['cols'])
        self.cola.SetColour(self.paramsimi['cola'])
        self.spin_width.SetValue(self.paramsimi['width'])
        self.spin_height.SetValue(self.paramsimi['height'])
        if 'cexfromchi' in self.paramsimi :
            self.checkit.SetValue(self.paramsimi['cexfromchi'])
        if 'sfromchi' in self.paramsimi :
            self.checki.SetValue(self.paramsimi['sfromchi'])
        if not self.paramsimi['first'] :
            self.check_coord.SetValue(self.paramsimi['keep_coord'])
            self.OnKeepCoords(wx.EVT_CHECKBOX)
        if self.paramsimi.get('bystar', False) :
            self.check_bystar.SetValue(True)
            self.stars = self.paramsimi['stars']
        self.slider_sphere.SetValue(self.paramsimi['alpha'])
        self.film.SetValue(self.paramsimi['film'])
        self.comcheck.SetValue(self.paramsimi['com'])
        self.choix_com.SetSelection(self.paramsimi['communities'])
        self.halo.SetValue(self.paramsimi['halo'])
        self.check_curved.SetValue(self.paramsimi.get('edgecurved', False))
        if self.paramsimi.get('word', False) and self.paramsimi['first'] :
            self.choice2.SetSelection(5)

    def ChangeCount(self, evt) :
        self.textcount.SetValue('%i' % self.listcol.GetSelectedItemCount())

    def OnCheck_s_size(self, evt):
        if self.check_s_size.GetValue() :
            if 'cexfromchi' in self.paramsimi :
                self.checki.SetValue(False)
            self.check2.SetValue(False)
            self.spin_tvmin.Enable(False)
            self.spin_tvmax.Enable(False)
            self.spin_tv.Enable(True)
        else :
            self.check2.SetValue(True)
            self.spin_tvmin.Enable(True)
            self.spin_tvmax.Enable(True)
            self.spin_tv.Enable(False)

    def OnCheck2(self, evt):
        if self.check2.GetValue():
            self.check_s_size.SetValue(False)
            if 'cexfromchi' in self.paramsimi :
                self.checki.SetValue(False)
            self.spin_tvmin.Enable(True)
            self.spin_tvmax.Enable(True)
            self.spin_tv.Enable(False)
        else :
            self.check_s_size.SetValue(True)
            self.spin_tvmin.Enable(False)
            self.spin_tvmax.Enable(False)
            self.spin_tv.Enable(True)

    def OnChecki(self, evt):
        if 'sfromchi' in self.paramsimi :
            if self.checki.GetValue() :
                self.check_s_size.SetValue(False)
                self.check2.SetValue(False)
                self.spin_tvmin.Enable(True)
                self.spin_tvmax.Enable(True)
                self.spin_tv.Enable(False)
            else :
                self.check_s_size.SetValue(True)
                #self.check2.SetValue(True)
                self.spin_tvmin.Enable(False)
                self.spin_tvmax.Enable(False)
                self.spin_tv.Enable(True)

    def OnCheckit(self,evt) :
        if 'cexfromchi' in self.paramsimi :
            if self.checkit.GetValue() :
                if self.check_vcex.GetValue() :
                    self.check_vcex.SetValue(False)

    def OnCheck_vcex(self, evt):
        if self.check_vcex.GetValue() :
            if 'checkit' in dir(self) :
                if self.checkit.GetValue() :
                    self.checkit.SetValue(False)

    def OnChangeType(self, event) :
        if event.GetInt() != 1 :
            self.spin_width.Enable(False)
            self.spin_height.Enable(False)
        else :
            self.spin_width.Enable(True)
            self.spin_height.Enable(True)
        if event.GetInt() not in [2,3] :
            self.film.Enable(False)
            self.slider_sphere.Enable(False)
        else :
            self.film.Enable(True)
            self.slider_sphere.Enable(True)

    def OnKeepCoords(self, event):
        if self.check_coord.GetValue() :
            self.choice1.SetSelection(self.paramsimi['coeff'])
            self.choice2.SetSelection(self.paramsimi['layout'])
            self.check_seuil.SetValue(self.paramsimi['seuil_ok'])
            self.spin_seuil.SetValue(self.paramsimi['seuil'])
            self.choice1.Disable()
            self.choice2.Disable()
            self.check_seuil.Disable()
            self.spin_seuil.Disable()
            #self.check_colch.SetValue(False)
            #self.check_colch.Disable()
        else :
            self.choice1.Enable(True)
            self.choice2.Enable(True)
            self.check_seuil.Enable(True)
            self.spin_seuil.Enable(True)
            #self.check_colch.Enable(True)


class PrepSimi :
    def __init__(self, parent, source, parametres, pathout, actives, indices_simi, wordlist = None, selected = None) :
        self.parametres = parametres
        self.etline = []
        self.dial = PrefSimi(parent, -1, self.parametres, indices_simi, wordlist = wordlist, selected = selected, actives = actives)
        self.dial.CenterOnParent()
        self.val = self.dial.ShowModal()
        if self.val == wx.ID_OK :
            if 'bystar' in self.parametres :
                if self.dial.check_bystar.GetValue() :
                    variables = treat_var_mod(self.parametres['stars'])
                    vardial = OptLexi(parent, force_chi = True)
                    vardial.listet = self.parametres['stars']
                    vardial.variables = [v for v in variables]
                    for et in vardial.variables :
                        vardial.list_box_1.Append(et)
                    nval = vardial.ShowModal()
                    if nval == wx.ID_OK :
                        if vardial.choice.GetSelection() == 1 :
                            listet = [vardial.listet[i] for i in vardial.list_box_1.GetSelections()]
                        else :
                            listet = variables[vardial.variables[vardial.list_box_1.GetSelections()[0]]]
                        self.dial.Destroy()
                        vardial.Destroy()
                        self.etline = source.corpus.make_etline(listet)
                        self.parametres['selectedstars'] = listet
                        self.parametres['listet'] = self.etline
                    else:
                        vardial.Destroy()
                        self.val = False
                        self.dial.Destroy()
            if self.val :
                last = self.dial.listcol.GetFirstSelected()
                lastl = [self.dial.listcol.GetFirstSelected()]
                indexes = [self.dial.listcol.getColumnText(self.dial.listcol.GetFirstSelected(),0)]
                while self.dial.listcol.GetNextSelected(last) != -1:
                    last = self.dial.listcol.GetNextSelected(last)
                    lastl.append(last)
                    indexes.append(self.dial.listcol.getColumnText(last,0))
                column = [actives.index(val) for val in indexes]
                column.sort()
                with open(pathout, 'w', encoding='utf8') as f :
                    f.write('\n'.join([repr(val) for val in column]))
                self.make_param()
                self.dial.Destroy()
        else :
            self.dial.Destroy()

    def make_param(self) :
        #self.select = self.dial.check_colch.GetValue()
        if self.parametres.get('first', True) :
            keep_coord = False
        else :
            keep_coord = self.dial.check_coord.GetValue()
        param = {'coeff' : self.dial.choice1.GetSelection(),
                          'layout' : self.dial.choice2.GetSelection(),
                          'type_graph' : self.dial.choice3.GetSelection(),
                          'arbremax' : self.dial.check1.GetValue(),
                          'coeff_tv' : self.dial.check_s_size.GetValue(),
                          'coeff_tv_nb' : self.dial.spin_tv.GetValue(),
                          'tvprop' : self.dial.check2.GetValue(),
                          'tvmin' : self.dial.spin_tvmin.GetValue(),
                          'tvmax' : self.dial.spin_tvmax.GetValue(),
                          'coeff_te' : self.dial.check3.GetValue(),
                          'coeff_temin' : self.dial.spin_temin.GetValue(),
                          'coeff_temax' : self.dial.spin_temax.GetValue(),
                          'label_e' : self.dial.check_elab.GetValue(),
                          'label_v' : self.dial.check_vlab.GetValue(),
                          'vcex' : self.dial.check_vcex.GetValue(),
                          'vcexmin' : self.dial.spin_vcexmin.GetValue(),
                          'vcexmax' : self.dial.spin_vcexmax.GetValue(),
                          'cex' : self.dial.spin_cex.GetValue(),
                          'seuil_ok' : self.dial.check_seuil.GetValue(),
                          'seuil' : self.dial.spin_seuil.GetValue(),
                          'cols' : self.dial.cols.GetColour(),
                          'cola' : self.dial.cola.GetColour(),
                          'width' : self.dial.spin_width.GetValue(),
                          'height' : self.dial.spin_height.GetValue(),
                          'first' : False,
                          'keep_coord' : keep_coord,
                          'alpha' : self.dial.slider_sphere.GetValue(),
                          'film' : self.dial.film.GetValue(),
                          'svg' : self.dial.choix_format.GetSelection(),
                          'com'  :self.dial.comcheck.GetValue(),
                          'communities' : self.dial.choix_com.GetSelection(),
                          'halo' : self.dial.halo.GetValue(),
                          'edgecurved' : self.dial.check_curved.GetValue(),
                          }
        if 'cexfromchi' in self.parametres :
            param['cexfromchi'] = self.dial.checkit.GetValue()
        if 'sfromchi' in self.parametres :
            param['sfromchi'] = self.dial.checki.GetValue()
        if 'vlabcolor' in self.parametres :
            param['vlabcolor'] = self.parametres['vlabcolor']
        if 'check_bystar' in dir(self.dial) :
            param['bystar'] = self.dial.check_bystar.GetValue()
            param['stars'] = self.parametres.get('stars', 0)
        self.parametres.update(param)

class CreateTgenDialog ( wx.Frame ):

    def __init__( self, parent, lemlist, tgen = None, tgens = None ):
        wx.Frame.__init__ ( self, parent, id = wx.ID_ANY, title = _('Tgen Creator'), pos = wx.DefaultPosition, size = wx.DefaultSize, style = wx.DEFAULT_FRAME_STYLE|wx.TAB_TRAVERSAL|wx.FRAME_FLOAT_ON_PARENT|wx.STAY_ON_TOP )
        self.ira = wx.GetApp().GetTopWindow()
        self.SetIcon(self.ira._icon)
        self.tgens = tgens
        self.edit = False
        self.parent = parent

        self.SetSizeHints( wx.DefaultSize, wx.DefaultSize )

        bSizer2 = wx.BoxSizer( wx.VERTICAL )

        namepan = wx.Panel(self, -1)

        fgSizer3 = wx.FlexGridSizer( 0, 2, 0, 0 )
        fgSizer3.SetFlexibleDirection( wx.BOTH )
        fgSizer3.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )

        self.m_staticText3 = wx.StaticText( namepan, wx.ID_ANY, _("Name"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText3.Wrap( -1 )

        fgSizer3.Add( self.m_staticText3, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_LEFT|wx.ALL, 5 )

        self.m_textCtrl1 = wx.TextCtrl( namepan, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_textCtrl1.SetMinSize( wx.Size( 250,-1 ) )

        fgSizer3.Add( self.m_textCtrl1, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        namepan.SetSizer( fgSizer3 )

        bSizer2.Add( namepan, 1, wx.EXPAND, 5 )

        self.ip = VItemsPicker(self,-1, lemlist, _('Forms'), _('Selection'))
        self.ip._source.SetMinSize( wx.Size( 350, 400 ) )

        bSizer2.Add( self.ip, 0, wx.ALL, 5 )

        butpanel = wx.Panel(self, -1)

        m_sdbSizer3 = wx.StdDialogButtonSizer()
        self.m_sdbSizer3OK = wx.Button( butpanel, wx.ID_OK )
        m_sdbSizer3.AddButton( self.m_sdbSizer3OK )
        self.m_sdbSizer3Cancel = wx.Button( butpanel, wx.ID_CANCEL )
        m_sdbSizer3.AddButton( self.m_sdbSizer3Cancel )
        m_sdbSizer3.Realize();

        butpanel.SetSizer( m_sdbSizer3 )

        bSizer2.Add( butpanel, 1, wx.EXPAND, 5 )


        self.SetSizer( bSizer2 )
        self.Layout()
        bSizer2.Fit( self )

        self.Centre( wx.BOTH )

        self.m_textCtrl1.Bind( wx.EVT_TEXT, self.OnTextEnter )
        self.ip.Bind(EVT_IP_SELECTION_CHANGED, self.OnSelectionChange)
        self.m_sdbSizer3OK.Bind(wx.EVT_BUTTON, self.OnClose)
        self.m_sdbSizer3Cancel.Bind(wx.EVT_BUTTON, self.OnCancel)

        #self.ip.SetItems(lemlist)
        self.m_sdbSizer3OK.Enable(False)

        if tgen is not None :
            self.m_textCtrl1.SetValue(tgen)
            self.ip._destData = dict([[i,[word,'']] for i, word in enumerate(tgens[tgen])])
            self.ip._SetDestItems()
            #self.ip.SetSelections(tgens[tgen])
            self.m_sdbSizer3OK.Enable(True)
            self.edit = True
        else :
            self.edit = False

    def __del__( self ):
        pass

    def OnTextEnter(self, evt):
        if self.m_textCtrl1.GetValue() != '' and self.m_textCtrl1.GetValue() not in self.tgens and self.ip.GetSelections() != []:
            self.m_sdbSizer3OK.Enable(True)
        else :
            self.m_sdbSizer3OK.Enable(False)
        if self.m_textCtrl1.GetValue() != '' and self.ip.GetSelections() and self.edit:
            self.m_sdbSizer3OK.Enable(True)

    def OnSelectionChange(self, evt):
        if self.ip.GetSelections() != [] and self.m_textCtrl1.GetValue() != '' and self.m_textCtrl1.GetValue() not in self.tgens :
            self.m_sdbSizer3OK.Enable(True)
        else :
            self.m_sdbSizer3OK.Enable(False)
        if self.m_textCtrl1.GetValue() != '' and self.ip.GetSelections() and self.edit:
            self.m_sdbSizer3OK.Enable(True)

    def OnClose(self, evt):
        self.Close()

    def OnCancel(self, evt):
        self.Destroy()

class TGenFrame ( wx.Frame ):

    def __init__( self, parent, corpus, Tgen ):
        wx.Frame.__init__ ( self, parent, id = wx.ID_ANY, title = "Tgen", pos = wx.DefaultPosition, size = wx.Size( -1, -1 ), style = wx.CLOSE_BOX|wx.DEFAULT_FRAME_STYLE|wx.TAB_TRAVERSAL|wx.STAY_ON_TOP )
        self.ira = wx.GetApp().GetTopWindow()
        self.SetIcon(self.ira._icon)
        self.Tgen = Tgen
        self.parent = parent
        self.corpus = corpus
        self.activetgen = None
        self.panel = wx.Panel(self, wx.ID_ANY)

        #self.SetSizeHints( wx.DefaultSize, wx.DefaultSize )

        panelsizer = wx.BoxSizer( wx.VERTICAL )

        bSizer1 = wx.BoxSizer( wx.VERTICAL )

        fgSizer1 = wx.FlexGridSizer( 0, 2, 0, 0 )
        fgSizer1.SetFlexibleDirection( wx.BOTH )
        fgSizer1.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )

        self.m_staticText1 = wx.StaticText( self.panel, wx.ID_ANY, "Tgen", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText1.Wrap( -1 )
        fgSizer1.Add( self.m_staticText1, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.m_staticText2 = wx.StaticText( self.panel, wx.ID_ANY, _("Content"), wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText2.Wrap( -1 )
        fgSizer1.Add( self.m_staticText2, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        tgensChoices = list(Tgen.tgen.keys())
        self.tgens = wx.ListBox( self.panel, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, tgensChoices, 0 )
        self.tgens.SetMinSize( wx.Size( 250,350 ) )

        fgSizer1.Add( self.tgens, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        tgencontentChoices = []
        self.tgencontent = wx.ListBox( self.panel, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, tgencontentChoices, 0|wx.VSCROLL )
        self.tgencontent.SetMinSize( wx.Size( 250,350 ) )

        fgSizer1.Add( self.tgencontent, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        fgSizer2 = wx.FlexGridSizer( 0, 2, 0, 0 )
        fgSizer2.SetFlexibleDirection( wx.BOTH )
        fgSizer2.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )

        self.but_new = wx.Button( self.panel, wx.ID_ANY, _("New..."), wx.DefaultPosition, wx.DefaultSize, 0 )
        fgSizer2.Add( self.but_new, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.but_del = wx.Button( self.panel, wx.ID_ANY, _("Delete"), wx.DefaultPosition, wx.DefaultSize, 0 )
        fgSizer2.Add( self.but_del, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )


        fgSizer1.Add( fgSizer2, 0, wx.EXPAND, 0 )

        fgSizer3 = wx.FlexGridSizer( 0, 2, 0, 0 )
        fgSizer3.SetFlexibleDirection( wx.BOTH )
        fgSizer3.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
        self.but_edit = wx.Button( self.panel, wx.ID_ANY, _("Edit"), wx.DefaultPosition, wx.DefaultSize, 0 )
        fgSizer3.Add( self.but_edit, 1, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        self.but_compute = wx.Button( self.panel, wx.ID_ANY, _("Compute"), wx.DefaultPosition, wx.DefaultSize, 0 )
        fgSizer3.Add( self.but_compute, 1, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        fgSizer1.Add( fgSizer3, 0, wx.EXPAND, 0 )

        bSizer1.Add( fgSizer1, 1, wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )


        m_sdbSizer2 = wx.StdDialogButtonSizer()
        self.m_sdbSizer2OK = wx.Button( self.panel, wx.ID_OK )
        m_sdbSizer2.AddButton( self.m_sdbSizer2OK )
        #self.m_sdbSizer2Cancel = wx.Button( self, wx.ID_CANCEL )
        #m_sdbSizer2.AddButton( self.m_sdbSizer2Cancel )
        m_sdbSizer2.Realize();

        bSizer1.Add( m_sdbSizer2, 0, wx.EXPAND, 5 )

        #panelsizer.Add(bSizer1, 0, wx.EXPAND, 5 )

        #panel.SetSizer( panelsizer )
        self.panel.SetSizer( bSizer1 )
        self.panel.Layout()
        bSizer1.Fit( self )

        self.Centre( wx.BOTH )

        # Connect Events
        self.tgens.Bind( wx.EVT_LISTBOX, self.GetContent )
        self.but_new.Bind( wx.EVT_BUTTON, self.OnNewTgen )
        self.but_del.Bind( wx.EVT_BUTTON, self.OnDeleteTgen )
        self.but_edit.Bind( wx.EVT_BUTTON, self.OnEditTgen )
        self.but_compute.Bind(wx.EVT_BUTTON, self.OnCompute)
        self.m_sdbSizer2OK.Bind( wx.EVT_BUTTON, self.OnOK )

    def __del__( self ):
        pass

    def GetContent( self, event ):
        tgen = event.GetString()
        if tgen != '' :
            self.tgencontent.Clear()
            for word in self.Tgen[tgen] :
                self.tgencontent.Append(word)

    def OnNewTgen( self, event, tgen = None ):
        if tgen is None :
            self.dial = CreateTgenDialog(self, dict([[i, [lem, self.corpus.lems[lem].freq, self.corpus.lems[lem].gram]] for i, lem in enumerate(self.corpus.lems.keys())]), tgens = self.Tgen.tgen)
        else :
            self.dial = CreateTgenDialog(self, dict([[i, [lem, self.corpus.lems[lem].freq, self.corpus.lems[lem].gram]] for i, lem in enumerate(self.corpus.lems.keys())]), tgen = tgen, tgens = self.Tgen.tgen)
            self.dial.ip._source.selected = dict(list(zip(self.Tgen.tgen[tgen], self.Tgen.tgen[tgen])))
            self.activetgen = tgen
        self.dial.Show()
        self.dial.Bind(wx.EVT_CLOSE, self.OnDialClose)

    def OnDeleteTgen( self, event ):
        if self.tgens.GetSelection() != -1 :
            tgens = self.tgens.GetItems()
            del self.Tgen.tgen[tgens[self.tgens.GetSelection()]]
            self.Tgen.write()
            self.tgens.Clear()
            self.tgencontent.Clear()
            for val in self.Tgen.tgen :
                self.tgens.Append(val)
        event.Skip()

    def OnEditTgen( self, event ):
        if self.tgens.GetSelection() != -1 :
            tgens = self.tgens.GetItems()
            tgen = tgens[self.tgens.GetSelection()]
            self.activetgen = tgen
            self.dial = CreateTgenDialog(self, dict([[i, [lem, self.corpus.lems[lem].freq, self.corpus.lems[lem].gram]] for i, lem in enumerate(self.corpus.lems.keys())]), tgen = tgen, tgens = self.Tgen.tgen)
            self.dial.Bind(wx.EVT_CLOSE, self.OnDialClose)
            self.dial.m_textCtrl1.Enable(False)
            self.dial.ip._source.selected = dict(list(zip(self.Tgen.tgen[tgen], self.Tgen.tgen[tgen])))
            self.dial.Show()
        event.Skip()

    def OnCompute(self, evt):
        ira = wx.GetApp().GetTopWindow()
        ira.tree.OnTgenCompute(evt)

    def OnOK(self, evt):
        self.Destroy()

    def OnDialClose(self, evt):
        if self.dial.edit :
            del self.Tgen.tgen[self.activetgen]
            self.tgens.Clear()
            self.tgencontent.Clear()
            for val in self.Tgen.tgen :
                self.tgens.Append(val)
        self.Tgen.tgen[self.dial.m_textCtrl1.GetValue()] = self.dial.ip.GetSelections()
        self.Tgen.write()
        self.tgens.Append(self.dial.m_textCtrl1.GetValue())
        self.dial.Destroy()
        evt.Skip()

class ExportMetaTable :
    def __init__(self, parent, corpus):
        self.ira = parent
        dial = PrefSimpleFile(self, self.ira, **{'mask' : '*.csv', 'title': _("metadata table")})
        dial.fbb.SetValue(corpus.pathout['metadata.csv'])
        dial.CenterOnParent()
        res = dial.ShowModal()
        if res == wx.ID_OK :
            fileout = dial.fbb.GetValue()
            dial.Destroy()
            corpus.export_meta_table(fileout)
            dlg = wx.MessageDialog(self.ira, _("Done !"), _("Export metadata"), wx.OK | wx.ICON_INFORMATION)
            dlg.CenterOnParent()
            dlg.ShowModal()
            dlg.Destroy()
        else :
            dial.Destroy()


def redosimi(self, evt) :
    with open(self.pathout['selected.csv'],'r', encoding='utf8') as f :
        selected = f.read()
    selected = [int(val) for val in selected.splitlines()]
    if self.actives is None :
        with open(self.pathout['actives.csv'], 'r', encoding='utf8') as f :
            self.actives = f.read()
        self.actives = self.actives.splitlines()#[act for act in self.actives.splitlines()]
    if os.path.exists(self.pathout['actives_nb.csv']) :
        with open(self.pathout['actives_nb.csv'], 'r', encoding='utf8') as f :
            act_nb = f.read()
            act_nb = act_nb.splitlines()
        dictcol = dict([[i, [self.actives[i], int(act_nb[i])]] for i, val in enumerate(self.actives)])
    else :
        dictcol = dict([[i, [act, self.corpus.getlemeff(act)]] for i, act in enumerate(self.actives)])
    #res = SelectColumn(self.ira, dictcol, self.actives, self.pathout['selected.csv'], selected = selected, dlg = True)
    #if res.ok :
    if evt is not None :
        prep = PrepSimi(self.ira, self, self.parametres,self.pathout['selected.csv'], self.actives, indices_simi, wordlist = dictcol, selected = selected)
    else :
        class EmptyBase(object): pass
        prep = EmptyBase()
        prep.val = wx.ID_OK
        prep.parametres = self.parametres
        order_actives = [[i, act, self.corpus.getlemeff(act)] for i, act in enumerate(self.actives)]
        order_actives = sorted(order_actives, key=itemgetter(2), reverse = True)
        with open(self.pathout['selected.csv'], 'w', encoding='utf8') as f :
            f.write('\n'.join([repr(order_actives[val][0]) for val in self.parametres['selected']]))
    if prep.val == wx.ID_OK or evt is None:
        self.parametres = prep.parametres

        script = PrintSimiScript(self)
        script.make_script()
        pid = exec_rcode(self.ira.RPath, script.scriptout, wait = True)
        check_Rresult(self.ira, pid)
        if self.parametres['type_graph'] in [1,3] :
            if self.parametres['svg'] :
                filename, ext = os.path.splitext(script.filename)
                fileout = filename + '.svg'
            elif self.parametres['type_graph'] == 3 :
                fileout = script.filename
                parametres = {'gexffile' :  fileout,
                  'dirout' : os.path.dirname(fileout),
                  'titre': 'Le titre',
                  #'nodemin': self.param['txt_min'],
                  #'nodemax': self.param['txt_max'],
                  #'bargraphw' : 60*int(self.param['clnb']),
                  }
                web = WebExport(self.ira, parametres)
                fileout = web.exportsimi()
            else :
                fileout = script.filename
            if os.path.exists(self.pathout['liste_graph']):
                graph_simi = read_list_file(self.pathout['liste_graph'])
                graph_simi.append([os.path.basename(fileout), script.txtgraph])
            else :
                graph_simi = [[os.path.basename(fileout), script.txtgraph]]
            self.fileout = fileout
            print_liste(self.pathout['liste_graph'], graph_simi)
        DoConf().makeoptions([self.parametres['type']], [self.parametres], self.pathout['Analyse.ira'])
        if evt is not None :
            if self.parametres['type_graph'] in [1,3] :
                if self.parametres['svg'] or self.parametres['type_graph'] == 3 :
                    self.graphpan.sizer_3.Add(hl.HyperLinkCtrl(self.graphpan.panel_1, -1, fileout, URL = fileout), 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
                else :
                    self.graphpan.sizer_3.Add(wx.StaticBitmap(self.graphpan.panel_1, -1, wx.Bitmap(fileout, wx.BITMAP_TYPE_ANY)), 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
                self.graphpan.sizer_3.Add(wx.StaticText(self.graphpan.panel_1,-1, script.txtgraph), 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
                self.graphpan.sizer_3.Fit(self.graphpan.panel_1)
                self.graphpan.Layout()
                self.graphpan.panel_1.Scroll(0,self.graphpan.panel_1.GetScrollRange(wx.VERTICAL))
