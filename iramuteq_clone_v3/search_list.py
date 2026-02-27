# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#----------------------------------------------------------------------------
# comes from ListCtrl.py from the demo tool of wxPython:
# Author:       Robin Dunn & Gary Dumer
#
# Created:
# Copyright:    (c) 1998 by Total Control Software
# Licence:      wxWindows license
#----------------------------------------------------------------------------

#------------------------------------
# import des modules python
#------------------------------------
import os
import sys
import io
import tempfile

#------------------------------------
# import des modules wx
#------------------------------------
import wx
import wx.lib.mixins.listctrl as listmix

#------------------------------------
# import des fichiers du projet
#------------------------------------
from functions import exec_rcode
from dialog import message, BarFrame
from chemins import ffr


class SearchList(wx.ListCtrl, listmix.ListCtrlAutoWidthMixin, listmix.ColumnSorterMixin): #wx.Panel, listmix.ColumnSorterMixin):

    def __init__(self, parent,gparent, dlist,first, nbactives, nbetoiles, add_dendro=True):
        wx.ListCtrl.__init__( self, parent, -1, style=wx.LC_REPORT|wx.LC_VIRTUAL|wx.LC_HRULES|wx.LC_VRULES)
        self.parent=parent
        self.gparent=gparent
        self.dlist=dlist
        self.add_dendro=add_dendro
        self.first = ['id','formes'] + first
        self.lenact = nbactives
        self.lensup = len(dlist) - (self.lenact + nbetoiles)
        #adding some art
        self.il = wx.ImageList(16, 16)
        a={"sm_up":"GO_UP","sm_dn":"GO_DOWN","w_idx":"WARNING","e_idx":"ERROR","i_idx":"QUESTION"}
        for k,v in list(a.items()):
            s="self.%s= self.il.Add(wx.ArtProvider.GetBitmap(wx.ART_%s,wx.ART_TOOLBAR,(16,16)))" % (k,v)
            exec(s)
        self.SetImageList(self.il, wx.IMAGE_LIST_SMALL)
        self.attr1 = wx.ItemAttr()
        self.attr1.SetBackgroundColour((220, 220, 220))
        self.attrsg = wx.ItemAttr()
        self.attrsg.SetBackgroundColour((230, 230, 230))
        self.attr2 = wx.ItemAttr()
        self.attr2.SetBackgroundColour((190, 249, 236))
        self.attr2s = wx.ItemAttr()
        self.attr2s.SetBackgroundColour((211, 252, 244))
        self.attr3 = wx.ItemAttr()
        self.attr3.SetBackgroundColour((245, 180, 180))
        self.attr3s = wx.ItemAttr()
        self.attr3s.SetBackgroundColour((245, 190, 190))
        tID = wx.NewId()
        self.dlist = dlist 
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected, self)
        #self.Bind(wx.EVT_LIST_COL_CLICK, self.OnColClick)
        # for wxMSW
        self.Bind(wx.EVT_COMMAND_RIGHT_CLICK, self.OnRightClick)
        # for wxGTK
        self.Bind(wx.EVT_RIGHT_UP, self.OnRightClick)

        #-----------------------------------------------------------
        first = ['id','formes']+first
        for i, name in enumerate(first) :
            self.InsertColumn(i, name, wx.LIST_FORMAT_LEFT)
        self.SetColumnWidth(0, wx.LIST_AUTOSIZE)
        for i in range(1,len(first)-1):
            self.SetColumnWidth(i, 130)
        self.itemDataMap = dlist
        self.itemIndexMap = list(dlist.keys())
        self.SetItemCount(len(dlist))
        listmix.ColumnSorterMixin.__init__(self, len(first)+2)
        self.SortListItems(0, True)

    # Used by the ColumnSorterMixin, see wx/lib/mixins/listctrl.py
    def GetListCtrl(self):
        return self

    # Used by the ColumnSorterMixin, see wx/lib/mixins/listctrl.py
    def GetSortImages(self):
        return (self.sm_dn, self.sm_up)

    def OnGetItemColumnImage(self, item, col):
        return -1

    def OnGetItemImage(self, item):
        pass

    def OnRightDown(self, event):
        x = event.GetX()
        y = event.GetY()
        item, flags = self.HitTest((x, y))
        if flags & wx.LIST_HITTEST_ONITEM:
            self.Select(item)
        event.Skip()

    def getColumnText(self, index, col):
        item = self.GetItem(index, col)
        return item.GetText()

    def OnGetItemText(self, item, col):
        index=self.itemIndexMap[item]
        s = self.itemDataMap[index][col]
        if isinstance(s, (int,float)):
            return str(s)
        else :
            return s

    def OnGetItemData(self, item) :
        index = self.itemIndexMap[item]
        s = self.itemDataMap[index]
        return s

    def OnItemSelected(self, event):
        self.currentItem = event.m_itemIndex
        event.Skip()

    def OnGetItemAttr(self, item):
        index=self.itemIndexMap[item]
        #genre=self.itemDataMap[index][2]
        if index < self.lenact :
            if item % 2 :
                return self.attr1
            else :
                return self.attrsg
        elif index >= self.lenact and index < (self.lenact + self.lensup) :
            if item % 2 :
                return self.attr2
            else :
                return self.attr2s
        elif index >= (self.lenact + self.lensup) :
            if item % 2 :
                return self.attr3
            else :
                return self.attr3s
        else :
            return None

#    def SortItems(self,sorter=cmp): ancienne version pour python 2
    def SortItems(self, sorter=None):
        listTemp = sorted(self.itemDataMap.items(),
            key=lambda x:x[1][self._col], reverse= (self._colSortFlag[self._col]!=True))
        dlist = dict([[line[0],line[1]] for line in listTemp])
        self.itemDataMap = dlist
        self.itemIndexMap = list(dlist.keys())
        self.Refresh() # redraw the list

    def OnRightClick(self, event):
        # only do this part the first time so the events are only bound once
        if not hasattr(self, "popupID1"):
            #self.popupID1 = wx.NewId()
            #self.popupID2 = wx.NewId()
            self.popupID3 = wx.NewId()
            if self.add_dendro :
                self.id_adddendro = wx.NewId()
                self.Bind(wx.EVT_MENU, self.ongraphdendro, id = self.id_adddendro)
            #self.Bind(wx.EVT_MENU, self.OnPopupOne, id=self.popupID1)
            #self.Bind(wx.EVT_MENU, self.OnPopupTwo, id=self.popupID2)
            self.Bind(wx.EVT_MENU, self.OnPopupThree, id=self.popupID3)
        # make a menu
        menu = wx.Menu()
        # add some items
        #menu.Append(self.popupID1, "Formes associées")
        #menu.Append(self.popupID2, "Concordancier")
        menu.Append(self.popupID3, "Graphique")
        if self.add_dendro :
            menu.Append(self.id_adddendro, "Graphique + dendrogramme")
        self.PopupMenu(menu)
        menu.Destroy()

    def getselectedwords(self) :
        words = [self.getColumnText(self.GetFirstSelected(), 6)]
        last = self.GetFirstSelected()
        while self.GetNextSelected(last) != -1:
            last = self.GetNextSelected(last)
            words.append(self.getColumnText(last, 6))
        return words

    def OnPopupOne(self, event):
        activenotebook = self.parent.nb.GetSelection()
        page = self.parent.nb.GetPage(activenotebook)
        corpus = page.corpus
        word = self.getColumnText(self.GetFirstSelected(), 0)
        lems = corpus.lems
        rep = []
        for forme in lems[word] :
            rep.append([forme, corpus.formes[forme][0]])
        win = message(self, "Formes associées", size=(300, 200))
        win.html = '<html>\n' + '<br>'.join([' : '.join([str(val) for val in forme]) for forme in rep]) + '\n</html>'
        win.HtmlPage.SetPage(win.html)
        win.Show(True)

    def OnPopupTwo(self, event):
        activenotebook = self.parent.nb.GetSelection()
        page = self.parent.nb.GetPage(activenotebook)
        item=self.getColumnText(self.GetFirstSelected(), 0)
        corpus = page.corpus
        win = message(self, "Concordancier", size=(600, 200))
        avap=60
        listmot = corpus.lems[item]
        uce_ok = [corpus.formes[forme][1] for forme in listmot]
        uce_ok = list(set([tuple(val) for line in uce_ok for val in line]))
        txt = '<h1>Concordancier</h1>'
        for uce in uce_ok:
            content = ' '+' '.join(corpus.ucis_paras_uces[uce[0]][uce[1]][uce[2]])+' '
            for form in listmot :
                sp = ''
                i = 0
                forme = ' ' + form + ' '
                while i < len(content):
                    coordword = content[i:].find(forme)
                    if coordword != -1 and i == 0:
                        txt += '<br><b>' + ' '.join(corpus.ucis[uce[0]][0]) + '</b><br>'
                        if coordword < avap:
                            sp = '&nbsp;' * (avap - coordword)
                            deb = i
                        else:
                            deb = i + coordword - avap
                        if len(content) < i + coordword + avap:
                            fin = len(content) - 1
                        else:
                            fin = i + coordword + avap
                        txt += '<TT>' + sp + content[deb:fin].replace(forme, '<font color=red>' + forme + '</font>') + '</TT><br>'
                        i += coordword + len(forme)
                        sp = ''
                    elif coordword != -1 and i != 0 :
                        if coordword < avap:
                            sp = '&nbsp;' * (avap - coordword)
                            deb = i
                        else:
                            deb = i + coordword - avap
                        if len(content) < i + coordword + avap:
                            fin = len(content) - 1
                        else:
                            fin = i + coordword + avap
                        txt += '<TT>' + sp + content[deb:fin].replace(forme, '<font color=red>' + forme + '</font>') + '</TT><br>'
                        i += coordword + len(forme)
                        sp = ''                   
                    else:
                        i = len(content)
                        sp = ''
        win.HtmlPage.SetPage(txt)
        win.Show(True)

    def OnPopupThree(self, event) :
        datas = [self.OnGetItemData(self.GetFirstSelected())]
        last = self.GetFirstSelected()
        while self.GetNextSelected(last) != -1:
            last = self.GetNextSelected(last)
            data = self.OnGetItemData(last)
            datas += [data]
        colnames = self.first[2:]
        rownames = [val[1] for val in datas]
        table = [[str(val) for val in line[2:]] for line in datas]
        BarFrame(self.parent.parent, table, colnames, rownames)

    def ongraphdendro(self, evt) :
        corpus = self.parent.corpus
        datas = [self.OnGetItemData(self.GetFirstSelected())]
        last = self.GetFirstSelected()
        while self.GetNextSelected(last) != -1:
            last = self.GetNextSelected(last)
            data = self.OnGetItemData(last)
            datas += [data]
        colnames = self.first[2:]
        rownames = [val[1] for val in datas]
        table = [[str(val) for val in line[2:]] for line in datas]
        tmpgraph = tempfile.mktemp(dir=self.parent.parent.TEMPDIR)
        BarFrame(self.parent.parent, table, colnames, rownames, tree = corpus.dictpathout['Rdendro'])
