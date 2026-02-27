# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#----------------------------------------------------------------------------
#comes from ListCtrl.py from the demo tool of wxPython:
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
import tempfile
from operator import itemgetter

import langue
langue.run()

#------------------------------------
# import des modules wx
#------------------------------------
import wx
import wx.lib.mixins.listctrl as listmix

#------------------------------------
# import des fichiers du projet
#------------------------------------
from functions import exec_rcode, doconcorde
from chemins import ffr
from PrintRScript import barplot
from dialog import SearchDial, message, BarGraphDialog, BarFrame




class ListForSpec(wx.ListCtrl, listmix.ListCtrlAutoWidthMixin, listmix.ColumnSorterMixin):

    def __init__(self, parent,gparent, dlist = {}, first = [], usefirst = False, menu = True):
        wx.ListCtrl.__init__( self, parent, -1, style=wx.LC_REPORT|wx.LC_VIRTUAL|wx.LC_HRULES|wx.LC_VRULES)
        self.parent=parent
        self.gparent=gparent
        self.dlist=dlist
        self.first = first
        self.tgen = False
        self.tgenlem = False
        if 'etoiles' in dir(self.gparent) and not usefirst :
            self.etoiles = self.gparent.etoiles
        else :
            self.etoiles = []
            for val in self.first :
                if val.startswith('X.') :
                    val = val.replace('X.', '*')
                self.etoiles.append(val)
        self.menu = menu
        #def start(self) :
        search_id = wx.NewId()
        self.parent.Bind(wx.EVT_MENU, self.onsearch, id = search_id)
        self.accel_tbl = wx.AcceleratorTable([(wx.ACCEL_CTRL, ord('F'), search_id)])
        self.SetAcceleratorTable(self.accel_tbl)
        self.il = wx.ImageList(16, 16)
        a={"sm_up":"GO_UP","sm_dn":"GO_DOWN","w_idx":"WARNING","e_idx":"ERROR","i_idx":"QUESTION"}
        for k,v in list(a.items()):
            s="self.%s= self.il.Add(wx.ArtProvider.GetBitmap(wx.ART_%s,wx.ART_TOOLBAR,(16,16)))" % (k,v)
            exec(s)
        self.SetImageList(self.il, wx.IMAGE_LIST_SMALL)
        tID = wx.NewId()
        self.attr1 = wx.ItemAttr()
        self.attr1.SetBackgroundColour((230, 230, 230))
        self.attr2 = wx.ItemAttr()
        self.attr2.SetBackgroundColour("light blue")
        self.attrselected = wx.ItemAttr()
        self.attrselected.SetBackgroundColour("red")
        self.selected = {}
        i=0
        for name in ['formes'] + self.etoiles :
            self.InsertColumn(i,name,wx.LIST_FORMAT_LEFT)
            i+=1
        self.SetColumnWidth(0, 200)
        for i in range(0,len(self.etoiles)):
            #size = self.checkcolumnwidth(len(self.etoiles[i]) * 20)
            #if size < 200 :
            #    size = 200
            #self.SetColumnWidth(i + 1, size)
            self.SetColumnWidth(i+1, wx.LIST_AUTOSIZE_USEHEADER)
            size = self.GetColumnWidth(i+1)
            if size < 80 :
                self.SetColumnWidth(i+1, 80)
            if size > 300 :
                self.SetColumnWidth(i+1, 300)
        self.itemDataMap = self.dlist
        self.itemIndexMap = list(self.dlist.keys())
        self.SetItemCount(len(self.dlist))
        listmix.ColumnSorterMixin.__init__(self, len(self.first) + 1)
        self.SortListItems(1, False)
        #-----------------------------------------------------------------------------------------
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected, self)
        self.Bind(wx.EVT_LIST_ITEM_ACTIVATED , self.OnPopupTwo, self)
        # for wxMSW
        self.Bind(wx.EVT_COMMAND_RIGHT_CLICK, self.OnRightClick)
        # for wxGTK
        self.Bind(wx.EVT_RIGHT_UP, self.OnRightClick)
#-----------------------------------------------------------------------------------------

    def OnGetItemColumnImage(self, item, col):
        return -1

    def OnGetItemImage(self, item):
        return -1

    def RefreshData(self, data):
        self.itemDataMap = data
        self.itemIndexMap = list(data.keys())
        self.SetItemCount(len(data))
        self.Refresh()

    def checkcolumnwidth(self, width) :
        if width < 80 :
            return 80
        else :
            return width

    def OnGetItemText(self, item, col):
        index=self.itemIndexMap[item]
        s = self.itemDataMap[index][col]
        if isinstance(s, (int, float)):
            return str(s)
        else:
            return s #modification pour python 3

    def ItemInSelected(self, item) :
        word = self.GetItemText(item, 0)
        if word in self.selected :
            return True
        else :
            return False

    def OnGetItemAttr(self, item):
        #print('get attr', item)
        #print(item)
        #word = self.itemDataMap[item][0]
        #index=self.itemIndexMap[item]
        ##word = self.GetItemText(item)
        #if word not in self.selected :
        ##if self.IsSelected(item) == False :
        ##if not self.ItemInSelected(item) :
            if item % 2 :
                return self.attr1
            else :
                return self.attr2
        #else :
        #    print(self.itemDataMap[item])
        #    print(item,self.dlist[item])
        #    return self.attrselected

    def GetItemByWord(self, word):
        return [ val for val in self.dlist if self.dlist[val][0] == word ][0]

    # Used by the ColumnSorterMixin, see wx/lib/mixins/listctrl.py
    def GetListCtrl(self):
        return self

    # Used by the ColumnSorterMixin, see wx/lib/mixins/listctrl.py
    def GetSortImages(self):
        return (self.sm_dn, self.sm_up)

    def OnRightDown(self, event):
        x = event.GetX()
        y = event.GetY()
        item, flags = self.HitTest((x, y))
        if flags & wx.LIST_HITTEST_ONITEM:
            self.Select(item)
        event.Skip()

    def GetString(self, evt):
        return self.getselectedwords()[0]

    def GetSelections(self):
        return self.getselectedwords()

    def getColumnText(self, index, col):
        item = self.GetItem(index, col)
        return item.GetText()

    def GetItemData(self, item) :
        index=self.itemIndexMap[item]
        s = self.itemDataMap[index]
        return s

#    def SortItems(self,sorter=cmp): # modification pour python 3
    def SortItems(self, sorter=None):
        listTemp = sorted(self.itemDataMap.items(),
            key=lambda x:x[1][self._col], reverse= (self._colSortFlag[self._col]!=True))
        dlist = dict([[line[0],line[1]] for line in listTemp])
        self.itemDataMap = dlist
        self.itemIndexMap = list(dlist.keys())
        self.Refresh() # redraw the list

    def OnItemSelected(self, event):
        self.currentItem = event.GetIndex() #event.m_itemIndex
        index=self.itemIndexMap[self.currentItem]
        event.Skip()

    def onsearch(self, evt) :
        self.dial = SearchDial(self, self, 0, True)
        self.dial.CenterOnParent()
        self.dial.Show()
        #self.dial.Destroy()

    def OnRightClick(self, event):
        if self.menu :
            # only do this part the first time so the events are only bound once
            if not hasattr(self, "popupID1"):
                self.popupID1 = wx.NewId()
                self.popupID2 = wx.NewId()
                self.popupID3 = wx.NewId()
                self.popup_Tgen_glob = wx.NewId()
                self.onmaketgen = wx.NewId()
                self.ID_stcaract = wx.NewId()
                self.id_tgendetails = wx.NewId()
                # on attache les événements aux éléments
                self.Bind(wx.EVT_MENU, self.OnPopupOne, id = self.popupID1)
                self.Bind(wx.EVT_MENU, self.OnPopupTwo, id = self.popupID2)
                self.Bind(wx.EVT_MENU, self.OnPopupThree, id = self.popupID3)
                self.Bind(wx.EVT_MENU, self.OnTgen_glob, id = self.popup_Tgen_glob)
                self.Bind(wx.EVT_MENU, self.OnMakeTgen, id = self.onmaketgen)
                self.Bind(wx.EVT_MENU, self.OnTgenDetails, id = self.id_tgendetails)
                #self.Bind(wx.EVT_MENU, self.onstcaract, id = self.ID_stcaract)
            # make a menu
            menu = wx.Menu()
            # add some items
            menu.Append(self.popupID1, _("Associated forms"))
            menu.Append(self.popupID2, _("Concordance"))
            menu.Append(self.popupID3, _("Graphic"))
            menu_stcaract = wx.Menu()
            self.menuid = {}
            if not self.tgen :
                for i, et in enumerate(self.etoiles) :
                    nid = wx.NewId()
                    self.menuid[nid] = i
                    menu_stcaract.Append(nid, et)
                    self.Bind(wx.EVT_MENU, self.onstcaract, id = nid)
                menu.Append(-1, _("Typical text segments"), menu_stcaract) #modifié
                menu.Append(self.onmaketgen, _("Make Tgen"))
            else :
                if self.tgenlem :
                    menu.Append(self.id_tgendetails, _('Tgen details'))
            self.PopupMenu(menu)
            menu.Destroy()

    def GetSelectionIndex(self) :
        index = [self.GetFirstSelected()]
        last = self.GetFirstSelected()
        while self.GetNextSelected(last) != -1:
            last = self.GetNextSelected(last)
            index.append(last)
        return index

    def getselectedwords(self) :
        words = [self.getColumnText(self.GetFirstSelected(), 0)]
        last = self.GetFirstSelected()
        while self.GetNextSelected(last) != -1:
            last = self.GetNextSelected(last)
            words.append(self.getColumnText(last, 0))
        return words

    def OnPopupOne(self, event):
        activenotebook = self.parent.nb.GetSelection()
        page = self.parent.nb.GetPage(activenotebook)
        corpus = page.corpus
        word = self.getselectedwords()[0]
        lems = corpus.getlems()
        rep = []
        for forme in lems[word].formes :
            rep.append([corpus.getforme(forme).forme, corpus.getforme(forme).freq])
        rep.sort(key = itemgetter(1), reverse = True)
        items = dict([[i, '<font face="courier">' + '\t:\t'.join([str(val) for val in forme]) + '</font>'] for i, forme in enumerate(rep)])
        win = message(self, items, _("Associated forms"), (300, 200))
        #win = message(self, u"Formes associées", (300, 200))
        #win.html = '<html>\n' + '<br>'.join([' : '.join([str(val) for val in forme]) for forme in rep]) + '\n</html>'
        #win.HtmlPage.SetPage(win.html)
        win.Show(True)

    def onstcaract(self, evt) :
        ind = self.menuid[evt.Id]
        limite = 50
        minind = 2
        activenotebook = self.parent.nb.GetSelection()
        page = self.parent.nb.GetPage(activenotebook)
        item=self.getColumnText(self.GetFirstSelected(), 0)
        corpus = page.corpus
        parametres = page.parametres
        paneff = self.gparent.ListPanEff
        panchi = self.gparent.ListPan
        et = self.etoiles[ind]
        uces = corpus.getucesfrometoile(et)
        self.la = [panchi.dlist[i][0] for i in range(0, len(panchi.dlist)) if panchi.dlist[i][ind+1] >= minind ]
        self.lchi = [panchi.dlist[i][ind+1] for i in range(0, len(panchi.dlist)) if panchi.dlist[i][ind+1] >= minind ]
        if max(self.lchi) == float('inf') :
            nchi = []
            for val in self.lchi :
                if val == float('inf') :
                    nchi.append(0)
                else :
                    nchi.append(val)
            nmax = max(nchi)
            nchi = [val if val != float('inf') else nmax + 2 for val in self.lchi]
            self.lchi = nchi
        tab = corpus.make_pondtable_with_classe(uces, self.la)
        tab.pop(0)
        ntab = [round(sum([(self.lchi[i] * word) for i, word in enumerate(line) if word != 0]),2) for line in tab]
        ntab2 = [[ntab[i], uces[i]] for i, val in enumerate(ntab) if ntab[i] != 0]
        del ntab
        ntab2.sort(reverse = True)
        ntab2 = ntab2[:limite]
        nuces = [val[1] for val in ntab2]
        ucis_txt, ucestxt = doconcorde(corpus, nuces, self.la)
        items = dict([[i, '<br>'.join([ucis_txt[i], '<table bgcolor = #1BF0F7 border=0><tr><td><b>score : %.2f</b></td></tr></table><br>' % ntab2[i][0], ucestxt[i]])] for i in range(0,len(ucestxt))])
        win = message(self, items, ' - '.join([_("Typical text segments"), "%s" % self.first[ind]]), (900, 600))
        win.Show(True)

    def OnPopupTwo(self, event):
        if 'nb' in dir(self.parent) :
            activenotebook = self.parent.nb.GetSelection()
            page = self.parent.nb.GetPage(activenotebook)
            corpus = page.corpus
        else :
            corpus = self.parent.parent.parent.corpus
        ira = wx.GetApp().GetTopWindow()
        item=self.getColumnText(self.GetFirstSelected(), 0)
        if not self.tgen :
            uce_ok = corpus.getlemuces(item)
            wordlist = [item]
        else :
            uce_ok = corpus.gettgenst(self.tgens[item])
            wordlist = [val for val in self.tgens[item] if val in corpus.lems]
        ucis_txt, ucestxt = doconcorde(corpus, uce_ok, wordlist)
        items = dict([[i, '<br><br>'.join([ucis_txt[i], ucestxt[i]])] for i in range(0,len(ucestxt))])
        win = message(ira, items, ' - '.join([_("Concordance"), "%s" % item]), (800, 500), uceids = uce_ok)
        win.Show(True)

    def getinf(self, txt) :
        if txt == float('Inf') :
            return 'Inf'
        elif txt == float('-Inf') :
            return '-Inf'
        else :
            return repr(txt)

    def OnPopupThree(self, event) :
        datas = [self.GetItemData(self.GetFirstSelected())]
        last = self.GetFirstSelected()
        while self.GetNextSelected(last) != -1:
            last = self.GetNextSelected(last)
            data = self.GetItemData(last)
            datas += [data]
        colnames = self.first
        table = [[self.getinf(val) for val in line[1:]] for line in datas]
        rownames = [val[0] for val in datas]
        BarFrame(self.parent, table, colnames, rownames)

    def OnTgen_glob(self, evt) :
        activenotebook = self.parent.nb.GetSelection()
        page = self.parent.nb.GetPage(activenotebook)
        corpus = page.corpus
        # préparation du script R
        tmpgraph = tempfile.mktemp(dir=self.parent.TEMPDIR)
        intxt = """
        load("%s")
        """ % corpus.dictpathout['RData']
        intxt += """
        Tgen.glob = NULL
        tovire <- NULL
        for (i in 1:ncol(dmf)) {
            Tgen.glob <- rbind(Tgen.glob,colSums(dmf[which(specf[,i] > 3),]))
            tovire <- append(tovire, which(specf[,i] > 3))
        }
        rownames(Tgen.glob) <- colnames(dmf)
        Tgen.table <- dmf[-unique(tovire),]
        Tgen.table<- rbind(Tgen.table, Tgen.glob)
        spec.Tgen.glob <- AsLexico2(Tgen.table)
        spec.Tgen.glob <- spec.Tgen.glob[[1]][((nrow(Tgen.table)-ncol(Tgen.table))+1):nrow(Tgen.table),]
        di <- spec.Tgen.glob
        """
        txt = barplot('', '', '', self.parent.RscriptsPath['Rgraph'], tmpgraph, intxt = intxt)
        # ecriture du script dans un fichier
        tmpscript = tempfile.mktemp(dir=self.parent.TEMPDIR)
        with open(tmpscript, 'w', encoding='utf8') as f :
            f.write(txt)
        # excution du script
        exec_rcode(self.parent.RPath, tmpscript, wait = True)
        win = MessageImage(self, -1, _("Graphic"), size=(700, 500),style = wx.DEFAULT_FRAME_STYLE)
        win.addsaveimage(tmpgraph)
        txt = "<img src='%s'>" % tmpgraph
        win.HtmlPage.SetPage(txt)
        win.Show(True)

    def OnTgenDetails(self, evt):
        if 'nb' in dir(self.parent) :
            activenotebook = self.parent.nb.GetSelection()
            page = self.parent.nb.GetPage(activenotebook)
            corpus = page.corpus
        else :
            corpus = self.parent.parent.parent.corpus
        ira = wx.GetApp().GetTopWindow()
        item=self.getColumnText(self.GetFirstSelected(), 0)
        wordlist = [val for val in self.tgens[item] if val in corpus.lems]
        wordlist = dict(list(zip(wordlist,wordlist)))
        res = dict([[val, self.tgenlem[val]] for val in self.tgenlem if self.tgenlem[val][0] in wordlist])
        win = ListLexFrame(self, ira, corpus, res, self.etoiles)
        win.Show()

    def OnMakeTgen(self, evt):
        self.parent.tree.OnTgenEditor(self.getselectedwords())


class ListLexFrame ( wx.Frame ):

    def __init__( self, parent, ira, corpus, data, columns ):
        wx.Frame.__init__ ( self, parent, id = wx.ID_ANY, title = wx.EmptyString, pos = wx.DefaultPosition, size = wx.Size( 500,300 ), style = wx.DEFAULT_FRAME_STYLE|wx.TAB_TRAVERSAL )
        self.SetSizeHints( wx.DefaultSize, wx.DefaultSize )
        bSizer1 = wx.BoxSizer( wx.VERTICAL )
        self.listlex = ListForSpec(self, ira, data, columns)
        bSizer1.Add( self.listlex, 5, wx.ALL|wx.EXPAND, 5 )
        m_sdbSizer1 = wx.StdDialogButtonSizer()
        self.m_sdbSizer1OK = wx.Button( self, wx.ID_OK )
        m_sdbSizer1.AddButton( self.m_sdbSizer1OK )
        self.m_sdbSizer1Cancel = wx.Button( self, wx.ID_CANCEL )
        m_sdbSizer1.AddButton( self.m_sdbSizer1Cancel )
        m_sdbSizer1.Realize();
        bSizer1.Add( m_sdbSizer1, 0, wx.EXPAND, 5 )
        self.SetSizer( bSizer1 )
        self.Layout()
        self.Centre( wx.BOTH )

    def __del__( self ):
        pass
