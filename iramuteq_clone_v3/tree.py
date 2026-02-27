# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import os
import webbrowser
import logging

import langue
langue.run()

#------------------------------------
# import des modules wx
#------------------------------------
import wx
import wx.lib.agw.customtreectrl as CT

#------------------------------------
# import des fichiers du projet
#------------------------------------
from openanalyse import OpenAnalyse
from corpus import Corpus, copycorpus
from tableau import Tableau, copymatrix
from functions import DoConf, GetTxtProfile, TGen, BugReport, open_folder, translateprofile, ReadProfileAsDico, write_translation_profile, progressbar, doconcorde
from profile_segment import ProfileSegment, ProfilType
from search_tools import SearchFrame
from dialog import PrefSimpleFile, PrefExport, SearchCorpus, translate_dialog, PrefUCECarac
from layout import open_antiprofil, TgenLayout
from guifunct import TGenFrame
from textaslexico import TgenSpec
from textreinert import TgenProf
from mergeclustergraph import MergeClusterGraph


log = logging.getLogger('iramuteq.tree')


def buildmenu(menu, parent_menu):
    for i in range(parent_menu.GetMenuItemCount()) :
        item = parent_menu.FindItemByPosition(i)
        itemid = item.GetId()
        itemtext = item.GetItemLabelText()
        itemicon = item.GetBitmap()
        nitem = wx.MenuItem(menu, itemid, itemtext)
        nitem.SetBitmap(itemicon)
        if item.IsSubMenu() :
            nmenu = wx.Menu()
            for val in item.GetSubMenu().GetMenuItems() :
                itemid = val.GetId()
                itemtext = val.GetItemLabelText()
                itemicon = val.GetBitmap()
                nitem = wx.MenuItem(menu, itemid, itemtext)
                nitem.SetBitmap(itemicon)
                nmenu.Append(nitem)
            menu.Append(-1, item.GetItemLabelText(), nmenu)
        else :
            menu.Append(nitem)


class InfoDialog ( wx.Dialog ):

    def __init__( self, parent, txt, parametres ):
        wx.Dialog.__init__ ( self, parent, id = wx.ID_ANY, title = "Informations", pos = wx.DefaultPosition, size = wx.DefaultSize, style = wx.DEFAULT_DIALOG_STYLE )
        if len(parametres) > 30 :
            nb = 4
        else :
            nb = 2
        self.SetSizeHints( wx.Size( 500,200 ), wx.DefaultSize )
        bSizer1 = wx.BoxSizer( wx.VERTICAL )
        self.m_panel2 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
        bSizer2 = wx.BoxSizer( wx.VERTICAL )
        self.m_staticText4 = wx.StaticText( self.m_panel2, wx.ID_ANY, txt, wx.DefaultPosition, wx.DefaultSize, 0 )
        self.m_staticText4.Wrap( -1 )
        bSizer2.Add( self.m_staticText4, 0, wx.ALL, 5 )
        self.m_panel2.SetSizer( bSizer2 )
        self.m_panel2.Layout()
        bSizer2.Fit( self.m_panel2 )
        bSizer1.Add( self.m_panel2, 0, wx.EXPAND |wx.ALL, 5 )
        self.m_panel1 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL|wx.VSCROLL )
        fgSizer1 = wx.FlexGridSizer( 0, nb, 0, 0 )
        fgSizer1.AddGrowableCol( 1 )
        if nb == 4 :
            fgSizer1.AddGrowableCol( 3 )
        fgSizer1.SetFlexibleDirection( wx.BOTH )
        fgSizer1.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_NONE )
        txtctrl = []
        for val in parametres :
            fgSizer1.Add( wx.StaticText( self.m_panel1, wx.ID_ANY, val[0], wx.DefaultPosition, wx.DefaultSize, 0 ), 0, wx.ALL, 0)
            #fgSizer1.Add( wx.StaticText( self.m_panel1, wx.ID_ANY, val[1], wx.DefaultPosition, wx.DefaultSize, 0 ), 0, wx.ALL, 0)
            txtctrl.append( wx.TextCtrl( self.m_panel1, wx.ID_ANY, val[1], wx.DefaultPosition, (450, 20), wx.TE_READONLY ) )
            txtctrl[-1].SetBackgroundColour('#DDE8EB')
            #wx.SystemSettings.GetColour(wx.SYS_COLOUR_GRAYTEXT))
            fgSizer1.Add( txtctrl[-1], 0, wx.ALL|wx.EXPAND, 0)
            #fgSizer1.Add( wx.StaticLine( self.m_panel1, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL ), 0, wx.EXPAND |wx.ALL, 0)
            #fgSizer1.Add( wx.StaticLine( self.m_panel1, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL ), 0, wx.EXPAND|wx.ALL, 0)
        self.m_panel1.SetSizer( fgSizer1 )
        self.m_panel1.Layout()
        fgSizer1.Fit( self.m_panel1 )
        bSizer1.Add( self.m_panel1, 1, wx.EXPAND|wx.ALL, 3 )
        m_sdbSizer1 = wx.StdDialogButtonSizer()
        self.m_sdbSizer1OK = wx.Button( self, wx.ID_OK )
        m_sdbSizer1.AddButton( self.m_sdbSizer1OK )
        m_sdbSizer1.Realize();
        bSizer1.Add( m_sdbSizer1, 0, wx.EXPAND, 5 )
        self.SetSizer( bSizer1 )
        self.Layout()
        bSizer1.Fit( self )
        self.Centre( wx.BOTH )

    def __del__( self ):
        pass


class LeftTree(CT.CustomTreeCtrl):

    def __init__(self, parent, id=wx.ID_ANY, pos=wx.DefaultPosition,
                 size=wx.DefaultSize,
                 style=wx.SUNKEN_BORDER|wx.WANTS_CHARS,
                 agwStyle=CT.TR_HIDE_ROOT|CT.TR_HAS_BUTTONS|CT.TR_HAS_VARIABLE_ROW_HEIGHT):
        CT.CustomTreeCtrl.__init__(self, parent, id, pos, size, style, agwStyle)

        ##################
        self.log = log
        alldata = dir(CT)
        treestyles = []
        events = []
        for data in alldata:
            if data.startswith("TR_"):
                treestyles.append(data)
            elif data.startswith("EVT_"):
                events.append(data)
        self.parent = parent
        self.ira = parent
        font = wx.Font(pointSize=self.ira.fontsize, family=wx.FONTFAMILY_DEFAULT, style=wx.FONTSTYLE_NORMAL, weight=wx.FONTWEIGHT_NORMAL)
        self.SetFont(font)
        self.events = events
        self.styles = treestyles
        self.item = None
        self.il = wx.ImageList(16, 16)
        self.ild = {}
        for img in self.ira.images_analyses :
            self.ild[img] = self.il.Add(self.ira.images_analyses[img])
        self.SetImageList(self.il)
        self.count = 0
        self.log = log
        self.history = parent.history
        self.h = self.history.history
        idopenfolder = wx.NewId()
        #accel_tbl = wx.AcceleratorTable([(wx.ACCEL_CTRL,  ord('E'),  idopenfolder)])
        self.Bind(wx.EVT_MENU, self.OnOpenFolder, id=idopenfolder)
        idsearchcorpus = wx.NewId()
        accel_tbl = wx.AcceleratorTable([(wx.ACCEL_CTRL,  ord('E'),  idopenfolder), (wx.ACCEL_CTRL,  ord('F'),  idsearchcorpus)])
        self.Bind(wx.EVT_MENU, self.OnSearchCorpus, id=idsearchcorpus)
        self.SetAcceleratorTable(accel_tbl)
        self.root = self.AddRoot("Iramuteq")
        if not(self.GetAGWWindowStyleFlag() & CT.TR_HIDE_ROOT):
            self.SetPyData(self.root, None)
            self.SetItemImage(self.root, 24, CT.TreeItemIcon_Normal)
            self.SetItemImage(self.root, 13, CT.TreeItemIcon_Expanded)
        self.textroot = self.AppendItem(self.root, _('Textual corpus'))
        self.SetPyData(self.textroot, {'uuid': 'textroot'})
        self.SetItemImage(self.textroot, self.ild['textroot'], CT.TreeItemIcon_Normal)
        self.SetItemImage(self.textroot, self.ild['textroot'], CT.TreeItemIcon_Expanded)
        for corpus in reversed(self.h) :
            child = self.AppendItem(self.textroot, corpus['corpus_name'])
            self.SetPyData(child, corpus)
            self.SetItemImage(child, self.ild['corpus'], CT.TreeItemIcon_Normal)
            self.SetItemImage(child, self.ild['corpus'], CT.TreeItemIcon_Expanded)
            if 'analyses' in corpus :
                for y in corpus['analyses'] :
                    last = self.AppendItem(child, y['name'], ct_type=0)
                    self.SetPyData(last, y)
                    if y['type'] in self.ild :
                        img = self.ild[y['type']]
                    else :
                        img = 24
                    self.SetItemImage(last, img, CT.TreeItemIcon_Normal)
                    self.SetItemImage(last, img, CT.TreeItemIcon_Expanded)
        self.matroot = self.AppendItem(self.root, _('Matrix'))
        self.SetPyData(self.matroot, {'uuid': 'matroot'})
        self.SetItemImage(self.matroot, self.ild['matroot'], CT.TreeItemIcon_Normal)
        self.SetItemImage(self.matroot, self.ild['matroot'], CT.TreeItemIcon_Expanded)
        orphmat = []
        for matrix in reversed(self.history.matrix) :
            if 'matrix_name' in matrix :
                child = self.AppendItem(self.matroot, matrix['matrix_name'])
                self.SetPyData(child, matrix)
                self.SetItemImage(child, self.ild['matrix'], CT.TreeItemIcon_Normal)
                self.SetItemImage(child, self.ild['matrix'], CT.TreeItemIcon_Expanded)
                if 'analyses' in matrix :
                    for y in matrix['analyses'] :
                        last = self.AppendItem(child, y['name'], ct_type=0)
                        self.SetPyData(last, y)
                        if y['type'] in self.ild :
                            img = self.ild[y['type']]
                        else :
                            img = 24
                        self.SetItemImage(last, img, CT.TreeItemIcon_Normal)
                        self.SetItemImage(last, img, CT.TreeItemIcon_Expanded)
            else :
                orphmat.append(matrix)
        self.Bind(wx.EVT_LEFT_DCLICK, self.OnLeftDClick)
        #self.Bind(wx.EVT_IDLE, self.OnIdle)
        self.eventdict = {'EVT_TREE_BEGIN_DRAG': self.OnBeginDrag, 'EVT_TREE_BEGIN_LABEL_EDIT': self.OnBeginEdit,
                          'EVT_TREE_BEGIN_RDRAG': self.OnBeginRDrag, 'EVT_TREE_DELETE_ITEM': self.OnDeleteItem,
                          'EVT_TREE_END_DRAG': self.OnEndDrag, 'EVT_TREE_END_LABEL_EDIT': self.OnEndEdit,
                          'EVT_TREE_ITEM_ACTIVATED': self.OnActivate, 'EVT_TREE_ITEM_CHECKED': self.OnItemCheck,
                          'EVT_TREE_ITEM_CHECKING': self.OnItemChecking, 'EVT_TREE_ITEM_COLLAPSED': self.OnItemCollapsed,
                          'EVT_TREE_ITEM_COLLAPSING': self.OnItemCollapsing, 'EVT_TREE_ITEM_EXPANDED': self.OnItemExpanded,
                          'EVT_TREE_ITEM_EXPANDING': self.OnItemExpanding, 'EVT_TREE_ITEM_GETTOOLTIP': self.OnToolTip,
                          'EVT_TREE_ITEM_MENU': self.OnItemMenu, 'EVT_TREE_ITEM_RIGHT_CLICK': self.OnRightDown,
                          'EVT_TREE_KEY_DOWN': self.OnKey, 'EVT_TREE_SEL_CHANGED': self.OnSelChanged,
                          'EVT_TREE_SEL_CHANGING': self.OnSelChanging, "EVT_TREE_ITEM_HYPERLINK": self.OnHyperLink}
        mainframe = wx.GetTopLevelParent(self)
        if not hasattr(mainframe, "leftpanel"):
            #self.Bind(CT.EVT_TREE_ITEM_EXPANDED, self.OnItemExpanded)
            #self.Bind(CT.EVT_TREE_ITEM_COLLAPSED, self.OnItemCollapsed)
            self.Bind(CT.EVT_TREE_SEL_CHANGED, self.OnSelChanged)
            self.Bind(CT.EVT_TREE_SEL_CHANGING, self.OnSelChanging)
            self.Bind(wx.EVT_RIGHT_DOWN, self.OnRightDown)
            self.Bind(wx.EVT_RIGHT_UP, self.OnRightUp)
        else:
            for combos in mainframe.treeevents:
                self.BindEvents(combos)
        if hasattr(mainframe, "leftpanel"):
            self.ChangeStyle(mainframe.treestyles)
        if not(self.GetAGWWindowStyleFlag() & CT.TR_HIDE_ROOT):
            self.SelectItem(self.root)
            self.Expand(self.root)

    def BindEvents(self, choice, recreate=False):
        value = choice.GetValue()
        text = choice.GetLabel()
        evt = "CT." + text
        binder = self.eventdict[text]
        if value == 1:
            if evt == "CT.EVT_TREE_BEGIN_RDRAG":
                self.Bind(wx.EVT_RIGHT_DOWN, None)
                self.Bind(wx.EVT_RIGHT_UP, None)
            self.Bind(eval(evt), binder)
        else:
            self.Bind(eval(evt), None)
            if evt == "CT.EVT_TREE_BEGIN_RDRAG":
                self.Bind(wx.EVT_RIGHT_DOWN, self.OnRightDown)
                self.Bind(wx.EVT_RIGHT_UP, self.OnRightUp)

    def ChangeStyle(self, combos):
        style = 0
        for combo in combos:
            if combo.GetValue() == 1:
                style = style | eval("CT." + combo.GetLabel())
        if self.GetAGWWindowStyleFlag() != style:
            self.SetAGWWindowStyleFlag(style)

    def OnCompareItems(self, item1, item2):
        t1 = self.GetItemText(item1)
        t2 = self.GetItemText(item2)
        if t1 < t2:
            return -1
        if t1 == t2:
            return 0
        return 1

    def OnIdle(self, event):
    #    if self.gauge:
    #        try:
    #            if self.gauge.IsEnabled() and self.gauge.IsShown():
    #                self.count = self.count + 1
    #                if self.count >= 50:
    #                    self.count = 0
    #                self.gauge.SetValue(self.count)
    #        except:
    #            self.gauge = None
        event.Skip()

    def CloseItem(self, itemParent = None, uuid = None) :
        if itemParent is None :
            itemParent = self.root
        child, cookie = self.GetFirstChild(itemParent)
        while child :
            pydata = self.GetPyData(child)
            if pydata['uuid'] == uuid :
                self.SetItemBold(child, False)
                break
            self.CloseItem(child, uuid)
            child, cookie = self.GetNextChild(itemParent, cookie)

    def GiveFocus(self, itemParent = None, uuid = None, bold = False) :
        if itemParent is None :
            itemParent = self.root
        child, cookie = self.GetFirstChild(itemParent)
        while child :
            pydata = self.GetPyData(child)
            if pydata['uuid'] == uuid :
                self.SelectItem(child)
                if bold :
                    self.SetItemBold(child, True)
                return
            self.GiveFocus(child, uuid, bold)
            child, cookie = self.GetNextChild(itemParent, cookie)

    def IsInTree(self, itemParent = None, uuid = None) :
        if itemParent is None :
            itemParent = self.root
        child, cookie = self.GetFirstChild(itemParent)
        while child :
            pydata = self.GetPyData(child)
            if pydata['uuid'] == uuid :
                return True
            self.GiveFocus(child, uuid)
            child, cookie = self.GetNextChild(itemParent, cookie)
        return False

    def OnRightDown(self, event):
        pt = event.GetPosition()
        item, flags = self.HitTest(pt)
        if item:
            self.item = item
            #self.log.info("OnRightClick: %s, %s, %s" % (self.GetItemText(item), type(item), item.__class__) + "\n")
            self.SelectItem(item)

    def OnRightUp(self, event):
        item = self.item
        if not item:
            event.Skip()
            return
        if not self.IsItemEnabled(item):
            event.Skip()
            return
        # Item Text Appearance
        ishtml = self.IsItemHyperText(item)
        back = self.GetItemBackgroundColour(item)
        fore = self.GetItemTextColour(item)
        isbold = self.IsBold(item)
        font = self.GetItemFont(item)
        # Icons On Item
        normal = self.GetItemImage(item, CT.TreeItemIcon_Normal)
        selected = self.GetItemImage(item, CT.TreeItemIcon_Selected)
        expanded = self.GetItemImage(item, CT.TreeItemIcon_Expanded)
        selexp = self.GetItemImage(item, CT.TreeItemIcon_SelectedExpanded)
        # Enabling/Disabling Windows Associated To An Item
        haswin = self.GetItemWindow(item)
        # Enabling/Disabling Items
        enabled = self.IsItemEnabled(item)
        # Generic Item's Info
        children = self.GetChildrenCount(item)
        itemtype = self.GetItemType(item)
        text = self.GetItemText(item)
        pydata = self.GetPyData(item)
        self.pydata = pydata
        self.current = item
        self.itemdict = {"ishtml": ishtml, "back": back, "fore": fore, "isbold": isbold,
                         "font": font, "normal": normal, "selected": selected, "expanded": expanded,
                         "selexp": selexp, "haswin": haswin, "children": children,
                         "itemtype": itemtype, "text": text, "pydata": pydata, "enabled": enabled}
        if not item in [self.textroot, self.matroot] :
            menu = wx.Menu()
            info = wx.MenuItem(menu, wx.ID_ANY, _("Informations"))
            info.SetBitmap(wx.ArtProvider.GetBitmap(wx.ART_INFORMATION, size = (16,16)))
            menu.Append(info)
            rename = wx.MenuItem(menu, wx.ID_ANY, _("Rename"))
            rename.SetBitmap(wx.ArtProvider.GetBitmap(wx.ART_TIP, size = (16,16)))
            menu.Append(rename)
            openfolder = wx.MenuItem(menu, wx.ID_ANY, _("Open directory"))
            openfolder.SetBitmap(wx.ArtProvider.GetBitmap(wx.ART_FOLDER_OPEN, size = (16,16)))
            menu.Append(openfolder)
            menu.AppendSeparator()
            if 'corpus_name' in pydata :
                buildmenu(menu, self.parent.text_menu)
                menu.AppendSeparator()
            elif 'matrix_name' in pydata :
                buildmenu(menu, self.parent.matrix_menu)
                menu.AppendSeparator()
            elif pydata.get('type', False) == 'alceste' and pydata['uuid'] in self.parent.history.opened :
                openmenu = wx.Menu()
                antipro = openmenu.Append(wx.ID_ANY, _("Antiprofiles"))
                menu.Append(wx.ID_ANY, _("Open ..."), openmenu)
                translate = menu.Append(wx.ID_ANY, _("Translate Profile"))
                profsr = menu.Append(wx.ID_ANY, _("Repeated segments profiles"))
                profgram = menu.Append(wx.ID_ANY, _("POS profiles"))
                stcaract = menu.Append(wx.ID_ANY, _("Typical text segments"))
                tgen = menu.Append(wx.ID_ANY, _("Tgen Editor"))
                computetgen = menu.Append(wx.ID_ANY, _("Compute Tgen"))
                #mergeclustergraph = menu.Append(wx.ID_ANY, _("Merge Cluster Graph"))
                export_corpus = menu.Append(wx.ID_ANY, _("Export corpus"))
                colored = menu.Append(wx.ID_ANY, _("Colored corpus"))
                navig = menu.Append(wx.ID_ANY, _("Navigator"))
                statclasse = menu.Append(wx.ID_ANY, _("Clusters statistics"))
                rapport = menu.Append(wx.ID_ANY, _("Report"))
                export_classes = menu.Append(wx.ID_ANY, _("Export clusters"))
                subcorpusfromcl = menu.Append(wx.ID_ANY, _("Sub corpus from clusters"))
                menu.AppendSeparator()
                self.Bind(wx.EVT_MENU, self.OpenAntipro, antipro)
                self.Bind(wx.EVT_MENU, self.OnTranslate, translate)
                self.Bind(wx.EVT_MENU, self.OnProfSR, profsr)
                self.Bind(wx.EVT_MENU, self.OnProfGram, profgram)
                self.Bind(wx.EVT_MENU, self.OnStCaract, stcaract)
                self.Bind(wx.EVT_MENU, self.OnTgenEditor, tgen)
                self.Bind(wx.EVT_MENU, self.OnTgenCompute, computetgen)
                #self.Bind(wx.EVT_MENU, self.OnMergeClusterGraph, mergeclustergraph)
                self.Bind(wx.EVT_MENU, self.OnExportCorpus, export_corpus)
                self.Bind(wx.EVT_MENU, self.OnColored, colored)
                self.Bind(wx.EVT_MENU, self.OnNavig, navig)
                self.Bind(wx.EVT_MENU, self.StatClasse, statclasse)
                self.Bind(wx.EVT_MENU, self.OnRapport, rapport)
                self.Bind(wx.EVT_MENU, self.OnExportClasses, export_classes)
                self.Bind(wx.EVT_MENU, self.OnSubCorpusFromClusters, subcorpusfromcl)
            elif pydata.get('type', False) == 'stat'  and pydata['uuid'] in self.parent.history.opened :
                export_dictionary =  menu.Append(wx.ID_ANY, _("Export dictionary"))
                export_lems =  menu.Append(wx.ID_ANY, _("Export lemma dictionary"))
                export_cut_corpus = menu.Append(wx.ID_ANY, _("Export segmented corpus"))
                self.Bind(wx.EVT_MENU, self.OnExportDictionary, export_dictionary)
                self.Bind(wx.EVT_MENU, self.OnExportLems, export_lems)
                self.Bind(wx.EVT_MENU, self.OnExportCutCorpus, export_cut_corpus)
                menu.AppendSeparator()
            elif pydata.get('type', False) == 'spec'  and pydata['uuid'] in self.parent.history.opened :
                tgen = menu.Append(wx.ID_ANY, _("Tgen Editor"))
                computetgen = menu.Append(wx.ID_ANY, _("Compute Tgen"))
                self.Bind(wx.EVT_MENU, self.OnTgenEditor, tgen)
                self.Bind(wx.EVT_MENU, self.OnTgenCompute, computetgen)
                menu.AppendSeparator()
            elif pydata.get('type', False) == 'reinertmatrix' and pydata['uuid'] in self.parent.history.opened :
                openmenu = wx.Menu()
                antipro = openmenu.Append(wx.ID_ANY, _("antiprofiles"))
                rapport = menu.Append(wx.ID_ANY, _("Report"))
                menu.Append(wx.ID_ANY, _("Open ..."), openmenu)
                self.Bind(wx.EVT_MENU, self.OpenAntipro, antipro)
                self.Bind(wx.EVT_MENU, self.OnRapport, rapport)
            itemdelete = wx.MenuItem(menu, wx.ID_ANY, _("Delete from history"))
            itemdelete.SetBitmap(wx.ArtProvider.GetBitmap(wx.ART_DELETE, size = (16,16)))
            menu.Append(itemdelete)
            #item11 = menu.Append(wx.ID_ANY, "Prepend An Item")
            #item12 = menu.Append(wx.ID_ANY, "Append An Item")
            #self.Bind(wx.EVT_MENU, self.OnItemBackground, item1)
            #self.Bind(wx.EVT_MENU, self.OnItemForeground, item2)
            #self.Bind(wx.EVT_MENU, self.OnItemBold, item3)
            #self.Bind(wx.EVT_MENU, self.OnItemFont, item4)
            #self.Bind(wx.EVT_MENU, self.OnItemHyperText, item5)
            #self.Bind(wx.EVT_MENU, self.OnEnableWindow, item6)
            #self.Bind(wx.EVT_MENU, self.OnDisableItem, item7)
            #self.Bind(wx.EVT_MENU, self.OnItemIcons, item8)
            self.Bind(wx.EVT_MENU, self.OnItemInfo, info)
            self.Bind(wx.EVT_MENU, self.OnRename, rename)
            self.Bind(wx.EVT_MENU, self.OnItemDelete, itemdelete)
            self.Bind(wx.EVT_MENU, self.OnOpenFolder, openfolder)
            #self.Bind(wx.EVT_MENU, self.OnItemPrepend, item11)
            #self.Bind(wx.EVT_MENU, self.OnItemAppend, item12)
            self.PopupMenu(menu)
            menu.Destroy()

    def getcorpus(self):
        busy = wx.BusyInfo(_("Please wait...Reading corpus"), self.parent)
        wx.SafeYield()
        if self.pydata['uuid'] in self.parent.history.openedcorpus :
            corpus = copycorpus(self.parent.history.openedcorpus[self.pydata['uuid']])
        elif 'corpus_name' in self.pydata :
            corpus = Corpus(self.parent, parametres = DoConf(self.pydata['ira']).getoptions('corpus'), read = True)
        else :
            cuuid = self.pydata['corpus']
            if cuuid in self.parent.history.openedcorpus :
                corpus = copycorpus(self.parent.history.openedcorpus[cuuid])
            else :
                irapath = self.parent.history.corpus[cuuid]['ira']
                corpus = Corpus(self.parent, parametres = DoConf(irapath).getoptions('corpus'), read = True)
        del busy
        return corpus

    def getmatrix(self):
        if 'matrix_name' in self.pydata :
            matrix = Tableau(self.parent, parametres = DoConf(self.pydata['ira']).getoptions('matrix'))
            matrix.open()
            return copymatrix(matrix)
        else :
            cuuid = self.pydata['matrix']
            matrix = Tableau(self.parent, parametres = DoConf(self.history.matrixanalyse[cuuid]['ira']).getoptions('matrix'))
            matrix.open()
            return copymatrix(matrix)

    def OnSpec(self, evt) :
        self.parent.OnTextSpec(evt, self.getcorpus())

    def OnStat(self, evt) :
        self.parent.OnTextStat(evt, self.getcorpus())

    def OnReinert(self, evt) :
        self.parent.OnTextReinert(evt, self.getcorpus())

    def OnPam(self, evt) :
        self.parent.OnPamSimple(evt, self.getcorpus())

    def OnSimiTxt(self, evt) :
        self.parent.OnSimiTxt(evt, self.getcorpus())

    def OnWordCloud(self, evt) :
        self.parent.OnWordCloud(evt, self.getcorpus())

#    def OnFreq(self, evt):
#        self.parent.OnFreq(evt, self.getmatrix())

#    def OnChiSquare(self, evt):
#        self.parent.OnChi2(evt, self.getmatrix())

#    def OnSimiTab(self, evt): 
#        self.parent.OnSimiTab(evt, self.getmatrix())
    
#    def OnProto(self, evt):
#        self.parent.OnProto(evt, self.getmatrix())
    
#    def OnSplitFromVar(self, evt):
#        self.parent.OnSplitVar(evt, self.getmatrix())
        
#    def OnCHDReinert(self, evt):
#        self.parent.OnCHDReinert(evt, self.getmatrix())
    
    #def OnSubTextFromMeta(self, evt):
    #    self.parent.OnSubText(self.getcorpus(), parametres = {'frommeta' : True})
    
    #def OnSubTextFromTheme(self, evt):
    #    self.parent.OnSubText(self.getcorpus(), parametres = {'fromtheme' : True})    

    def OnProfSR(self, evt) :
        ProfileSegment(self.parent, self.page.dictpathout, self.page.parametres, self.page.corpus)

    def OnProfGram(self, evt) :
        ProfilType(self.parent, self.page.corpus, self.page.parametres)

    def OnMergeClusterGraph(self, evt) :
        MergeClusterGraph(self.parent, self.page.corpus, self.page.parametres)
        print('merge done !')

    def OnStCaract(self, evt) :
        dial = PrefUCECarac(self, self.parent)
        dial.CenterOnParent()
        if self.page.parametres['classif_mode'] != 2 :
            uci = False
        else :
            uci = True
        if dial.ShowModal() == wx.ID_OK :
            limite = dial.spin_eff.GetValue()
            atype = dial.radio_type.GetSelection()
            dial.Destroy()
            corpus = self.page.corpus
            dlg = progressbar(self.ira, maxi = len(corpus.lc))
            dlg.Update(1, 'wait...')
            for i in range(0, len(corpus.lc)) :
                page = self.page.ProfNB.GetPage(i)
                rcl = page.cl - 1
                dlg.Update(i, 'Cluster %i' % (i+1))
                uces = corpus.lc[rcl]
                tab = corpus.make_table_with_classe(uces, page.la, uci = uci)
                tab.pop(0)
                if atype == 0 :
                    ntab = [round(sum([page.lchi[j] for j, word in enumerate(line) if word == 1]),2) for line in tab]
                else :
                    ntab = [round(sum([page.lchi[j] for j, word in enumerate(line) if word == 1])/float(sum(line)),2) if sum(line)!=0 else 0 for line in tab]
                ntab2 = [[ntab[j], uces[j]] for j, val in enumerate(ntab)]
                del ntab
                ntab2.sort(reverse = True)
                ntab2 = ntab2[:limite]
                nuces = [val[1] for val in ntab2]
                ucis_txt, ucestxt = doconcorde(corpus, nuces, page.la, uci = uci)
                items = ['<br>'.join([ucis_txt[j], '<table bgcolor = #1BF0F7 border=0><tr><td><b>score : %.2f</b></td></tr></table><br>' % ntab2[j][0], ucestxt[j]]) for j, uce in enumerate(nuces)]
                filename = self.page.pathout['st_caract_cl_%i.html' % (rcl+1)]
                with open(filename, 'w', encoding='utf8') as f :
                    f.write('\n'.join(items))
            dlg.Destroy()

    def OnExportCorpus(self, evt) :
        dial = PrefExport(self, self.parent)
        dial.fbb.SetValue(os.path.join(os.path.dirname(self.page.dictpathout['ira']), 'export_corpus.txt'))
        dial.CenterOnParent()
        res = dial.ShowModal()
        if res == wx.ID_OK :
            if dial.radio_type.GetSelection() == 0 : alc = True
            else : alc = False
            if dial.radio_lem.GetSelection() == 0 : lem = True
            else : lem = False
            if self.page.parametres['classif_mode'] != 2 :
                uci = False
            else :
                uci = True
            self.page.corpus.export_corpus_classes(dial.fbb.GetValue(), alc = alc, lem = lem, uci = uci)
            msg = "Fini !"
            dial.Destroy()
            dlg = wx.MessageDialog(self.parent, msg, "Export", wx.OK | wx.ICON_INFORMATION)
            dlg.CenterOnParent()
            dlg.ShowModal()
            dlg.Destroy()

    def OnExportCutCorpus(self, evt) :
        uci = False
        fileout = os.path.join(os.path.dirname(self.page.pathout['ira']), 'segmented_corpus.txt')
        txt = self.page.corpus.make_cut_corpus(uci = uci)
        with open(fileout, 'w', encoding='utf8') as f :
            f.write(txt)
        msg = '\n'.join([_("Done !"), fileout])
        dlg = wx.MessageDialog(self.parent, msg, _("Segmented corpus"), wx.OK | wx.ICON_INFORMATION)
        dlg.CenterOnParent()
        dlg.ShowModal()
        dlg.Destroy()

    def OnColored(self, evt) :
        dial = PrefSimpleFile(self, self.parent, **{'mask' : '*.html', 'title': _("Colored corpus")})
        dial.fbb.SetValue(os.path.join(os.path.dirname(self.page.dictpathout['ira']), 'corpus_couleur.html'))
        dial.CenterOnParent()
        res = dial.ShowModal()
        if res == wx.ID_OK :
            fileout = dial.fbb.GetValue()
            dial.Destroy()
            if self.page.parametres['classif_mode'] != 2 :
                uci = False
            else :
                uci = True
            txt = self.page.corpus.make_colored_corpus(uci = uci)
            with open(fileout, 'w', encoding='utf8') as f :
                f.write(txt)
            msg = ' !\n'.join([_("Done"), _("Open in a web browser ?")])
            dlg = wx.MessageDialog(self.parent, msg, "Corpus en couleur", wx.NO | wx.YES | wx.ICON_QUESTION)
            dlg.CenterOnParent()
            if dlg.ShowModal() == wx.ID_YES :
                webbrowser.open(fileout)
            dlg.Destroy()

    def OnNavig(self, evt):
        if 'FrameSearch' not in dir(self.page) :
            self.page.FrameSearch = SearchFrame(self.parent, -1, _("Search ..."), self.page.corpus)
        self.page.FrameSearch.Show()

    def StatClasse(self, evt):
        dial = PrefSimpleFile(self, self.parent, **{'mask' : '*.csv', 'title': _("Clusters statistics")})
        dial.fbb.SetValue( os.path.join(os.path.dirname(self.page.dictpathout['ira']), 'stat_par_classe.csv'))
        dial.CenterOnParent()
        res = dial.ShowModal()
        if res == wx.ID_OK :
            fileout = dial.fbb.GetValue()
            dial.Destroy()
            self.page.corpus.get_stat_by_cluster(fileout)
            msg = "Fini !"
            dlg = wx.MessageDialog(self.parent, msg, _("Clusters statistics"), wx.OK | wx.ICON_INFORMATION)
            dlg.CenterOnParent()
            if dlg.ShowModal() == wx.ID_OK :
                dlg.Destroy()

    def OpenAntipro(self, evt) :
        find = False
        for i in range(0, self.page.TabChdSim.GetPageCount()) :
            page = self.page.TabChdSim.GetPage(i)
            if self.page.TabChdSim.GetPageText(i) == _("Antiprofiles") :
                self.page.TabChdSim.SetSelection(i)
                find = True
                break
        if not find :
            open_antiprofil(self.page, self.page.dictpathout['ANTIPRO_OUT'], 'utf8')
            self.page.TabChdSim.SetSelection(self.page.TabChdSim.GetPageCount() - 1)

    def OnTranslate(self, evt) :
        dial = translate_dialog(self.parent)
        dial.CenterOnParent()
        res = dial.ShowModal()
        if res == wx.ID_OK :
            to_l = dial.getto_l()
            from_l = dial.getfrom_l()
            dial.Destroy()
            busy = wx.BusyInfo(_("Please wait..."), self.parent)
            wx.SafeYield()
            prof, lems = translateprofile(self.page.corpus, ReadProfileAsDico(self.page.dictpathout['PROFILE_OUT'], True, self.parent.syscoding), lf=from_l, lt=to_l)
            write_translation_profile(prof, lems, to_l, self.page.dictpathout)
            open_antiprofil(self.page, prof, 'utf8', title = _('Translation') + ' %s' % to_l, translation = True, lems=lems)
            del busy
            self.page.lems = lems
            self.page.TabChdSim.SetSelection(self.page.TabChdSim.GetPageCount() - 1)
        else :
            dial.Destroy()

    def OnRapport(self, evt) :
        dial = PrefSimpleFile(self, self.parent, **{'mask' : '*.txt', 'title': _("Report")})
        dial.fbb.SetValue(self.page.dictpathout['rapport'])
        dial.CenterOnParent()
        res = dial.ShowModal()
        if res == wx.ID_OK :
            fileout = dial.fbb.GetValue()
            dial.Destroy()
            with open(fileout, 'w', encoding='utf8') as f :
                f.write(self.page.debtext + '\n' + GetTxtProfile(self.page.DictProfile, self.page.cluster_size))
            msg = "Fini !"
            dlg = wx.MessageDialog(self.parent, msg, _("Report"), wx.OK | wx.ICON_INFORMATION)
            dlg.CenterOnParent()
            dlg.ShowModal()
            dlg.Destroy()
        else :
            dial.Destroy()

    def OnExportDictionary(self, evt) :
        corpus = self.page.corpus
        corpus.export_dictionary(self.page.pathout['dictionary.csv'], self.parent.syscoding)
        log.info('export dictionary %s' % self.page.pathout['dictionary.csv'])
        dial = wx.MessageDialog(self.parent, self.page.pathout['dictionary.csv'], 'Export', wx.OK)
        dial.ShowModal()
        dial.Destroy()

    def OnExportLems(self, evt) :
        corpus = self.page.corpus
        corpus.export_lems(self.page.pathout['lemmes.csv'], self.parent.syscoding)
        log.info('export lemmes %s' % self.page.pathout['lemmes.csv'])
        dial = wx.MessageDialog(self.parent, self.page.pathout['lemmes.csv'], 'Export', wx.OK)
        dial.ShowModal()
        dial.Destroy()

    def OnTgenEditor(self, evt):
        corpus = self.page.corpus
        tgenpath = os.path.join(self.page.parametres['pathout'], 'tgen.csv')
        tgen = TGen(path = tgenpath, encoding = self.parent.syscoding)
        if os.path.exists(tgenpath) :
            tgen.read(tgenpath)
        if isinstance(evt, list) :
            i = 0
            while 'tgen%i' %i in tgen.tgen :
                i += 1
            tgenname = 'tgen%i' %i
            tgen.tgen[tgenname] = evt
        tgenframe = TGenFrame(self.parent, corpus, tgen)
        tgenframe.Show()
        if isinstance(evt, list) :
            tgenframe.OnNewTgen(None, tgen = tgenname)

    def OnTgenCompute(self, evt):
        corpus = self.page.corpus
        tgenpath = os.path.join(self.page.parametres['pathout'], 'tgen.csv')
        if not os.path.exists(tgenpath) :
            message = wx.MessageDialog(self.parent, _("No TGen yet !"), style = wx.ICON_EXCLAMATION | wx.OK)
            message.ShowModal()
            message.Destroy()
        else :
            self.page.parametres['tgenpath'] = tgenpath
            tgen = TGen(path = tgenpath, encoding = 'utf8')
            if self.page.parametres['type'] == 'spec' :
                self.page.parametres['etoiles'] = self.page.etoiles
                TgenSpec(self.parent, corpus, self.page.parametres)
            elif self.page.parametres['type'] == 'alceste' :
                TgenProf(self.parent, corpus, self.page.parametres, self.page.cluster_size)
            TgenLayout(self.page)

    def OnExportClasses(self, event):
        corpus = self.page.corpus
        if self.page.parametres['classif_mode'] != 2 :
            uci = False
        else :
            uci = True
        busy = wx.BusyInfo(_("Please wait..."), self.parent)
        wx.SafeYield()
        for i in range(1, self.page.parametres['clnb'] + 1) :
            corpus.export_classe(self.page.pathout['classe_%i_export.txt' % i], i, uci = uci)
        del busy
        dial = wx.MessageDialog(self, self.page.pathout['classe_x_export.txt'], "Export", wx.OK|wx.ICON_INFORMATION)
        dial.ShowModal()
        dial.Destroy()

    def OnSubCorpusFromClusters(self, evt):
        self.parent.OnSubText(evt, corpus = self.getcorpus(), parametres = {'fromclusters' : True, 'clnb': self.page.parametres['clnb'], 'lc' : self.page.corpus.lc})

    def OnRename(self, event):
        pydata = self.itemdict['pydata']
        if 'matrix_name' in pydata :
            name = 'matrix_name'
        elif 'corpus_name' in pydata :
            name = 'corpus_name'
        else :
            name = 'name'
        oldname = pydata[name]
        dlg = wx.TextEntryDialog(self, _("New Name"), _('Rename'), oldname)
        if dlg.ShowModal() == wx.ID_OK:
            newname = dlg.GetValue()
            dlg.Destroy()
            pydata[name] = newname
            Totconf = DoConf(configfile=pydata['ira'])
            conf = Totconf.getoptions()
            conf[name] = newname
            Totconf.makeoptions(Totconf.getsections(), [conf])
            self.history.update(pydata)
            self.SetItemText(self.current, newname)
            self.EnsureVisible(self.current)

    def OnOpenFolder(self, evt):
        try :
            open_folder(os.path.dirname(self.pydata['ira']))
        except :
            print('cannot open folder %s' % self.pydata.get('ira', 'noirapath'))

    def GetCorpusByName(self, corpus_name) :
        return [corpus for corpus in self.h if corpus_name in corpus['corpus_name']]

    def OnSearchCorpus(self, evt):
        searchframe = SearchCorpus(self.ira, self, None, None)
        searchframe.Show()
        #res = self.GetCorpusByName('Ministres')
        #print res

    def SetContentBackground(self, itemParent = None, uuid = None, color = True) :
        if itemParent is None :
            itemParent = self.root
        child, cookie = self.GetFirstChild(itemParent)
        while child :
            pydata = self.GetPyData(child)
            if pydata['uuid'] == uuid :
                self.SelectItem(child)
                if color :
                    self.SetItemBackgroundColour(child, wx.Colour(15,234,56))
                return
            self.SetContentBackground(child, uuid, color)
            child, cookie = self.GetNextChild(itemParent, cookie)

    def OnItemBackground(self, event):
        colourdata = wx.ColourData()
        colourdata.SetColour(self.itemdict["back"])
        dlg = wx.ColourDialog(self, colourdata)
        dlg.GetColourData().SetChooseFull(True)
        if dlg.ShowModal() == wx.ID_OK:
            data = dlg.GetColourData()
            col1 = data.GetColour().Get()
            self.SetItemBackgroundColour(self.current, col1)
        dlg.Destroy()

    def OnItemForeground(self, event):
        colourdata = wx.ColourData()
        colourdata.SetColour(self.itemdict["fore"])
        dlg = wx.ColourDialog(self, colourdata)
        dlg.GetColourData().SetChooseFull(True)
        if dlg.ShowModal() == wx.ID_OK:
            data = dlg.GetColourData()
            col1 = data.GetColour().Get()
            self.SetItemTextColour(self.current, col1)
        dlg.Destroy()

    def OnItemBold(self, event):
        self.SetItemBold(self.current, not self.itemdict["isbold"])

    def OnItemFont(self, event):
        data = wx.FontData()
        font = self.itemdict["font"]
        if font is None:
            font = wx.SystemSettings_GetFont(wx.SYS_DEFAULT_GUI_FONT)
        data.SetInitialFont(font)
        dlg = wx.FontDialog(self, data)
        if dlg.ShowModal() == wx.ID_OK:
            data = dlg.GetFontData()
            font = data.GetChosenFont()
            self.SetItemFont(self.current, font)
        dlg.Destroy()

    def OnItemHyperText(self, event):
        self.SetItemHyperText(self.current, not self.itemdict["ishtml"])

    def OnEnableWindow(self, event):
        enable = self.GetItemWindowEnabled(self.current)
        self.SetItemWindowEnabled(self.current, not enable)

    def OnDisableItem(self, event):
        self.EnableItem(self.current, False)

    def OnItemIcons(self, event):
        bitmaps = [self.itemdict["normal"], self.itemdict["selected"],
                   self.itemdict["expanded"], self.itemdict["selexp"]]
        wx.BeginBusyCursor()
        dlg = TreeIcons(self, -1, bitmaps=bitmaps)
        wx.EndBusyCursor()
        dlg.ShowModal()

    def SetNewIcons(self, bitmaps):
        self.SetItemImage(self.current, bitmaps[0], CT.TreeItemIcon_Normal)
        self.SetItemImage(self.current, bitmaps[1], CT.TreeItemIcon_Selected)
        self.SetItemImage(self.current, bitmaps[2], CT.TreeItemIcon_Expanded)
        self.SetItemImage(self.current, bitmaps[3], CT.TreeItemIcon_SelectedExpanded)

    def OnItemInfo(self, event):
        itemtext = self.itemdict["text"]
        numchildren = str(self.itemdict["children"])
        itemtype = self.itemdict["itemtype"]
        pydata = self.itemdict['pydata']
        #if 'analyses' in pydata :
        #    toshow = dict([[val, pydata[val]] for val in pydata if val not in['analyses', 'isload']])
        #else :
        toshow = pydata['ira']
        toshow = DoConf(toshow).getoptions()
        txt = DoConf().totext(toshow)
        parametres = [val.split('\t\t:') for val in txt.splitlines()]
        parametres.sort()
        if itemtype == 0:
            itemtype = "Normal"
        elif itemtype == 1:
            itemtype = "CheckBox"
        else:
            itemtype = "RadioButton"
        dlg = InfoDialog(self, itemtext, parametres)
        dlg.CenterOnParent()
        dlg.ShowModal()
        dlg.Destroy()

    def OnItemDelete(self, event):
        strs = "Are You Sure You Want To Delete Item " + self.GetItemText(self.current) + "?"
        dlg = wx.MessageDialog(None, strs, 'Deleting Item', wx.OK | wx.CANCEL | wx.ICON_QUESTION)
        if dlg.ShowModal() in [wx.ID_NO, wx.ID_CANCEL]:
            dlg.Destroy()
            return
        dlg.Destroy()
        pydata = self.itemdict['pydata']
        if 'corpus_name' in pydata :
            self.history.delete(pydata, True)
        else :
            self.history.delete(pydata)
        self.DeleteChildren(self.current)
        self.Delete(self.current)
        self.current = None

    def OnItemPrepend(self, event):
        dlg = wx.TextEntryDialog(self, "Please Enter The New Item Name", 'Item Naming', 'Python')
        if dlg.ShowModal() == wx.ID_OK:
            newname = dlg.GetValue()
            newitem = self.PrependItem(self.current, newname)
            self.EnsureVisible(newitem)
        dlg.Destroy()

    def AddAnalyse(self, parametres, itemParent = None, bold = True) :
        uuid = parametres.get('corpus', None)
        if uuid is not None :
            if itemParent is None :
                itemParent = self.textroot
            child, cookie = self.GetFirstChild(itemParent)
            corpus = None
            while child :
                pydata = self.GetPyData(child)
                if pydata['uuid'] == uuid :
                    corpus = child
                    break
                self.GiveFocus(child, uuid)
                child, cookie = self.GetNextChild(itemParent, cookie)
            #item = self.AppendItem(child, parametres['name'])
            if corpus is not None : 
                item = self.AppendItem(corpus, parametres['name'])
            else :
                item = self.AppendItem(self.textroot, parametres['name'])
        else :
            item = self.AppendItem(self.matroot, parametres['name'])
        self.SetPyData(item, parametres)
        if parametres['type'] in self.ild :
            img = self.ild[parametres['type']]
        else :
            img = 24
        self.SetItemImage(item, img, CT.TreeItemIcon_Normal)
        self.SetItemImage(item, 13, CT.TreeItemIcon_Expanded)
        self.SetItemBold(item, bold)
        self.SelectItem(item)

    def AddMatAnalyse(self, parametres, itemParent = None, bold = True) :
        uuid = parametres.get('matrix', None)
        if uuid is not None :
            if itemParent is None :
                itemParent = self.matroot
            child, cookie = self.GetFirstChild(itemParent)
            matrix = None
            while child :
                pydata = self.GetPyData(child)
                if pydata['uuid'] == uuid :
                    matrix = child
                    break
                self.GiveFocus(child, uuid)
                child, cookie = self.GetNextChild(itemParent, cookie)
            #item = self.AppendItem(child, parametres['name'])
            if matrix is not None : 
                item = self.AppendItem(matrix, parametres['name'])
            else :
                item = self.AppendItem(self.matroot, parametres['name'])
        self.SetPyData(item, parametres)
        if parametres['type'] in self.ild :
            img = self.ild[parametres['type']]
        else :
            img = 24
        self.SetItemImage(item, img, CT.TreeItemIcon_Normal)
        self.SetItemImage(item, 13, CT.TreeItemIcon_Expanded)
        self.SetItemBold(item, bold)
        self.SelectItem(item)  

    def OnItemAppend(self, item, select = True):
        if 'corpus_name' in item :
            child = self.InsertItem(self.textroot, 0, item['corpus_name'])
        else :
            child = self.InsertItem(self.matroot, 0, item['matrix_name'])
        self.SetPyData(child, item)
        if item['type'] in self.ild :
            img = self.ild[item['type']]
        else :
            img = 24
        self.SetItemImage(child, img, CT.TreeItemIcon_Normal)
        self.SetItemImage(child, img, CT.TreeItemIcon_Expanded)
        if select :
            self.history.addtab(item)
            self.SetItemBold(child, True)
            self.SelectItem(child)
        #dlg = wx.TextEntryDialog(self, "Please Enter The New Item Name", 'Item Naming', 'Python')
        #if dlg.ShowModal() == wx.ID_OK:
        #    newname = dlg.GetValue()
        #    newitem = self.AppendItem(self.current, newname)
        #    self.EnsureVisible(newitem)
        #dlg.Destroy()

    def OnBeginEdit(self, event):
        #self.log.info("OnBeginEdit" + "\n")
        # show how to prevent edit...
        item = event.GetItem()
        if item and self.GetItemText(item) == "The Root Item":
            wx.Bell()
            #self.log.info("You can't edit this one..." + "\n")
            # Lets just see what's visible of its children
            cookie = 0
            root = event.GetItem()
            (child, cookie) = self.GetFirstChild(root)
            while child:
                #self.log.info("Child [%s] visible = %d" % (self.GetItemText(child), self.IsVisible(child)) + "\n")
                (child, cookie) = self.GetNextChild(root, cookie)
            event.Veto()

    def OnEndEdit(self, event):
        #self.log.info("OnEndEdit: %s %s" %(event.IsEditCancelled(), event.GetLabel()))
        # show how to reject edit, we'll not allow any digits
        for x in event.GetLabel():
            if x in string.digits:
                #self.log.info(", You can't enter digits..." + "\n")
                event.Veto()
                return
        self.log.info("\n")

    def OnLeftDClick(self, event):
        pt = event.GetPosition()
        item, flags = self.HitTest(pt)
        if item is not None :
            pydata = self.GetPyData(item)
            if pydata['uuid'] in self.parent.history.opened :
                for i in range(self.parent.nb.GetPageCount()) :
                    page = self.parent.nb.GetPage(i)
                    if 'parametres' in dir(page) :
                        if page.parametres['uuid'] == pydata['uuid'] :
                            self.parent.nb.SetSelection(i)
                            break
            elif pydata['uuid'] in ['textroot', 'matroot'] :
                pass
            else :
                if os.path.exists(pydata['ira']) :
                    busy = wx.BusyInfo(_("Please wait..."), self.parent)
                    #wx.SafeYield()
                    try :
                        OpenAnalyse(self.parent, pydata)
                        del busy
                        self.SetItemBold(item, True)
                        self.OnSelChanged(pydata = pydata)
                    except :
                        del busy
                        BugReport(self.ira)
                else :
                    wx.MessageBox(_("This file does not exist : %s" % pydata['ira']), 'Information', wx.ICON_EXCLAMATION | wx.STAY_ON_TOP )
        #if item and (flags & CT.TREE_HITTEST_ONITEMLABEL):
        #    if self.GetAGWWindowStyleFlag() & CT.TR_EDIT_LABELS:
        #        self.log.info("OnLeftDClick: %s (manually starting label edit)"% self.GetItemText(item) + "\n")
                #self.EditLabel(item)
        #    else:
        #        pydata = self.GetPyData(item)
        #        print pydata
        #        self.log.info("OnLeftDClick: Cannot Start Manual Editing, Missing Style TR_EDIT_LABELS\n")
        event.Skip()

    def OnItemExpanded(self, event):
        item = event.GetItem()
        if item:
            self.log.info("OnItemExpanded: %s" % self.GetItemText(item) + "\n")

    def OnItemExpanding(self, event):
        item = event.GetItem()
        if item:
            self.log.info("OnItemExpanding: %s" % self.GetItemText(item) + "\n")
        event.Skip()

    def OnItemCollapsed(self, event):
        item = event.GetItem()
        if item:
            self.log.info("OnItemCollapsed: %s" % self.GetItemText(item) + "\n")

    def OnItemCollapsing(self, event):
        item = event.GetItem()
        if item:
            self.log.info("OnItemCollapsing: %s" % self.GetItemText(item) + "\n")
        event.Skip()

    def OnSelChanged(self, event = None, pydata = None):
        if event is not None :
            item = event.GetItem()
            pydata = self.GetPyData(item)
        if pydata is not None :
            if 'corpus_name' in pydata or 'corpus' in pydata :
                #self.ira.ShowMenu('matrix', False)
                self.ira.ShowMenu('text', True)
            if 'matrix_name' in pydata or 'matrix' in pydata:
                #self.ira.ShowMenu('text', False)
                self.ira.ShowMenu('matrix', True)
            if 'uuid' in pydata :
                if pydata['uuid'] in ['textroot', 'matroot'] :
                    self.ira.ShowMenu('text', False)
                    self.ira.ShowMenu('matrix', False)
            self.pydata = pydata
            if pydata['uuid'] in self.parent.history.opened :
                for i in range(self.parent.nb.GetPageCount()) :
                    self.page = self.parent.nb.GetPage(i)
                    if 'parametres' in dir(self.page) :
                        if self.page.parametres['uuid'] == pydata['uuid'] :
                            self.parent.nb.SetSelection(i)
                            wx.CallAfter(self.parent.nb.SendSizeEvent)
                            #self.parent.Refresh()
                            break
        #self.parent._mgr.Update()
        #if event is not None :
        #    event.Skip()

    def OnSelChanging(self, event):
        item = event.GetItem()
        olditem = event.GetOldItem()
        if item:
            if not olditem:
                olditemtext = "None"
            else:
                olditemtext = self.GetItemText(olditem)
            #self.log.info("OnSelChanging: From %s" % olditemtext + " To %s" % self.GetItemText(item) + "\n")
        #event.Skip()

    def OnBeginDrag(self, event):
        self.item = event.GetItem()
        if self.item:
            self.log.info("Beginning Drag..." + "\n")
            event.Allow()

    def OnBeginRDrag(self, event):
        self.item = event.GetItem()
        if self.item:
            self.log.info("Beginning Right Drag..." + "\n")
            event.Allow()

    def OnEndDrag(self, event):
        self.item = event.GetItem()
        if self.item:
            self.log.info("Ending Drag!" + "\n")
        event.Skip()

    def OnDeleteItem(self, event):
        item = event.GetItem()
        if not item:
            return
        self.log.info("Deleting Item: %s" % self.GetItemText(item) + "\n")
        event.Skip()

    def OnItemCheck(self, event):
        item = event.GetItem()
        self.log.info("Item " + self.GetItemText(item) + " Has Been Checked!\n")
        event.Skip()

    def OnItemChecking(self, event):
        item = event.GetItem()
        self.log.info("Item " + self.GetItemText(item) + " Is Being Checked...\n")
        event.Skip()

    def OnToolTip(self, event):
        item = event.GetItem()
        if item:
            event.SetToolTip(wx.ToolTip(self.GetItemText(item)))

    def OnItemMenu(self, event):
        item = event.GetItem()
        if item:
            self.log.info("OnItemMenu: %s" % self.GetItemText(item) + "\n")
        event.Skip()

    def OnKey(self, event):
        keycode = event.GetKeyCode()
        keyname = keyMap.get(keycode, None)
        if keycode == wx.WXK_BACK:
            self.log.info("OnKeyDown: HAHAHAHA! I Vetoed Your Backspace! HAHAHAHA\n")
            return
        if keyname is None:
            if "unicode" in wx.PlatformInfo:
                keycode = event.GetUnicodeKey()
                if keycode <= 127:
                    keycode = event.GetKeyCode()
                keyname = "\"" + chr(event.GetUnicodeKey()) + "\""
                if keycode < 27:
                    keyname = "Ctrl-%s" % chr(ord('A') + keycode-1)
            elif keycode < 256:
                if keycode == 0:
                    keyname = "NUL"
                elif keycode < 27:
                    keyname = "Ctrl-%s" % chr(ord('A') + keycode-1)
                else:
                    keyname = "\"%s\"" % chr(keycode)
            else:
                keyname = "unknown (%s)" % keycode
        self.log.info("OnKeyDown: You Pressed '" + keyname + "'\n")
        event.Skip()

    def OnActivate(self, event):
        if self.item:
            self.log.info("OnActivate: %s" % self.GetItemText(self.item) + "\n")
        event.Skip()

    def OnHyperLink(self, event):
        item = event.GetItem()
        if item:
            self.log.info("OnHyperLink: %s" % self.GetItemText(self.item) + "\n")

    def OnTextCtrl(self, event):
        char = chr(event.GetKeyCode())
        self.log.info("EDITING THE TEXTCTRL: You Wrote '" + char + \
                       "' (KeyCode = " + str(event.GetKeyCode()) + ")\n")
        event.Skip()

    def OnComboBox(self, event):
        selection = event.GetEventObject().GetValue()
        self.log.info("CHOICE FROM COMBOBOX: You Chose '" + selection + "'\n")
        event.Skip()
