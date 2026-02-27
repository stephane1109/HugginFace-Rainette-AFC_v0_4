# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2022 Pierre Ratinaud
#License: GNU/GPL

import os
import wx
import wx.xrc
from wx.lib.splitter import MultiSplitterWindow
from listlex import *
import pickle
import json

import langue
langue.run()


class CategoDict :
    def __init__(self, pathout = None):
        self.pathout = pathout
        self.cate = self.readjson()
        self.lenwords = len(self.cate['TOCATE']) + len([word for categorie in self.cate['CATE'] for word in self.cate['CATE'][categorie][1]])

    def readjson(self):
        if self.pathout is not None :
            with open(self.pathout['cate.json'], 'r', encoding='utf8') as f :
                cate = json.load(f)
        else :
            cate = {'TOCATE' : {'word1': 3, 'word2' : 2, 'word3' : 5}, 'CATE': {'cat1' : [34,{'word6':30, 'word7':4}], 'cat2' : [20,{'word20':20}]}}
        return cate

    def save(self) :
        with open(self.pathout['cate.json'], 'w', encoding='utf8') as f :
            f.write(json.dumps(self.cate, indent=4))

    def exportdict(self):
        pass

    def getcate(self) :
        cate = []
        i = 0
        for val in self.cate['CATE'] :
            cate.append([i, [val, self.cate['CATE'][val][0]]])
            i += 1
        return dict(cate)

    def getwordstocate(self) :
        words = []
        i = 0
        for val in self.cate['TOCATE'] :
            words.append([i, [val, self.cate['TOCATE'][val]]])
            i+= 1
        return dict(words)

    def getcatewords(self, cat) :
        catewords = []
        i = 0
        if cat not in self.cate['CATE'] :
            return {}
        for val in self.cate['CATE'][cat][1] :
            catewords.append([i, [val, self.cate['CATE'][cat][1][val]]])
            i += 1
        return dict(catewords)

    def getwordscate(self) :
        wc = {}
        for word in self.cate['TOCATE'] :
            wc[word] = word
        for categorie in self.cate['CATE'] :
            for word in self.cate['CATE'][categorie][1] :
                wc[word] = categorie
        return wc

    def addwordincate(self, categorie, word, eff) :
        self.cate['CATE'][categorie][1][word] = eff
        self.cate['CATE'][categorie][0] += eff
        del(self.cate['TOCATE'][word])

    def addwordinwords(self, categorie, word, eff) :
        self.cate['TOCATE'][word] = eff
        self.cate['CATE'][categorie][0] -= eff
        del(self.cate['CATE'][categorie][1][word])
        if self.cate['CATE'][categorie][0] == 0 :
            del(self.cate['CATE'][categorie])

    def findcatefromword(self, word) :
        for categorie in self.cate['CATE'] :
            if word in self.cate['CATE'][categorie][1] :
                return categorie
        return None

    def changewordcate(self, newcate, word, eff) :
        oldcat = self.findcatefromword(word)
        del(self.cate['CATE'][oldcat][1][word])
        self.cate['CATE'][oldcat][0] -= eff
        self.cate['CATE'][newcate][1][word] = eff
        self.cate['CATE'][newcate][0] += eff
        if self.cate['CATE'][oldcat][0] == 0 :
            del(self.cate['CATE'][oldcat])

    def addcatefromwordtocate(self, word, eff) :
        if word in self.cate['CATE'] :
            return False
        else :
            self.cate['CATE'][word]=[eff,{word:eff}]
            del(self.cate['TOCATE'][word])
            return True

    def addcatefromscratch(self) :
        i = 0
        while "NewCategory_%i" %i in self.cate['CATE'] :
            i += 1
        newcate = "NewCategory_%i" %i
        self.cate['CATE'][newcate] = [0, {}]

    def addcatefromwordcate(self, word, eff) :
        if word in self.cate['CATE'] :
            return False
        else :
            oldcat = self.findcatefromword(word)
            self.cate['CATE'][word]=[eff,{word:eff}]
            del(self.cate['CATE'][oldcat][1][word])
            self.cate['CATE'][oldcat][0] -= eff
            if self.cate['CATE'][oldcat][0] == 0 :
                del(self.cate['CATE'][oldcat])
            return True

    def delcate(self, categorie) :
        for word in self.cate['CATE'][categorie][1] :
            self.cate['TOCATE'][word] = self.cate['CATE'][categorie][1][word]
        del(self.cate['CATE'][categorie])

    def loadcate(self, infile) :
        if self.cate['CATE'] != {} :
            return False
        with open(infile, 'r', encoding='utf8') as f :
            newcate = json.load(f)
        for categorie in newcate['CATE'] :
            self.cate['CATE'][categorie] = [0,{}]
            for word in newcate['CATE'][categorie][1] :
                if word in self.cate['TOCATE'] :
                    self.cate['CATE'][categorie][1][word] = self.cate['TOCATE'][word]
                    self.cate['CATE'][categorie][0] += self.cate['TOCATE'][word]
                    del(self.cate['TOCATE'][word])

    def makestat(self) :
        totocat = sum([self.cate['TOCATE'][word] for word in self.cate['TOCATE']])
        nbtocat = len(self.cate['TOCATE'])
        nbcate = len(self.cate['CATE'])
        totcate = sum([self.cate['CATE'][categorie][0] for categorie in self.cate['CATE']])
        lenwordincate = len([word for categorie in self.cate['CATE'] for word in self.cate['CATE'][categorie][1]])
        return nbtocat, totocat, nbcate, totcate, lenwordincate


#cate = CategoDict()


#class ElCategorizator ( wx.Panel ):
#
#    def __init__( self, parent, pathout, tableau, id = wx.ID_ANY, pos = wx.DefaultPosition, size = wx.Size( 500,300 ), style = wx.TAB_TRAVERSAL, name = wx.EmptyString ):
#        wx.Panel.__init__ ( self, parent, id = id, pos = pos, size = size, style = style, name = name )
#        self.pathout = pathout
#        self.parent = parent
#        self.tableau = tableau
#
#        self.cate = CategoDict(self.pathout)
#        gsizer =  wx.BoxSizer( wx.VERTICAL )
#
#        bSizer1 = wx.BoxSizer( wx.HORIZONTAL )
#
#        self.m_listToCate = ListForWords(self, dlist = self.cate, first = ['eff'])
#        bSizer1.Add( self.m_listToCate, 2, wx.ALL|wx.EXPAND, 5 )
#
#        self.m_listCate = ListForCate(self, dlist = self.cate, first = ['eff'])
#        bSizer1.Add( self.m_listCate, 1, wx.ALL|wx.EXPAND, 5 )
#
#        self.m_listCateWords = ListForCateWords(self, dlist = self.cate, first = ['eff'])
#        bSizer1.Add( self.m_listCateWords, 1, wx.ALL|wx.EXPAND, 5 )
#
#        bSizer2 = wx.BoxSizer( wx.HORIZONTAL )
#
#        self.butsave = wx.Button( self, wx.ID_SAVE, u"Save", wx.DefaultPosition, wx.DefaultSize, 0 )
#        bSizer2.Add( self.butsave, 0, wx.ALL, 5 )
#
#        self.butcsv = wx.Button( self, wx.ID_ANY, u"Export Columns", wx.DefaultPosition, wx.DefaultSize, 0 )
#        bSizer2.Add( self.butcsv, 0, wx.ALL, 5 )
#
#        self.butdict = wx.Button( self, wx.ID_ANY, u"Export dictonary", wx.DefaultPosition, wx.DefaultSize, 0 )
#        bSizer2.Add( self.butdict, 0, wx.ALL, 5 )
#
#        self.butload = wx.Button( self, wx.ID_ANY, u"Load a categorization", wx.DefaultPosition, wx.DefaultSize, 0 )
#        bSizer2.Add( self.butload, 0, wx.ALL, 5 )
#
#        self.butaddcate = wx.Button( self, wx.ID_ANY, u"Add a category", wx.DefaultPosition, wx.DefaultSize, 0 )
#        bSizer2.Add( self.butaddcate, 0, wx.ALL, 5 )
#
#
#        bSizer3 = wx.BoxSizer( wx.HORIZONTAL )
#
#        self.nbword = """Words : {:d} ({:d}) | """
#
#        self.stat = """ Words to categorize : {:d} ({}%) - {:d} ({}%) -- Categories : {:d} - {:d} ({}%) - {:d} ({}%)"""
##        nbtocat, totocat, nbcate, totcate = self.cate.makestat()
##        lenwords = self.cate.lenwords
##        totwords = totocat + totcate
##        prtocat = repr(nbtocat/lenwords)
##        prtotocat = repr(totocat/totwords)
##        prcate = repr(totcate/totwords)
#        self.wordtxt = wx.StaticText(self, -1, "")
#        bSizer3.Add( self.wordtxt, 0, wx.ALL, 5 )
#        self.stattxt = wx.StaticText(self, -1, "")
#        bSizer3.Add( self.stattxt, 0, wx.ALL, 5 )
#
#
#        gsizer.Add( bSizer2, 0, wx.EXPAND, 5 )
#        gsizer.Add( bSizer1, 2, wx.EXPAND, 5 )
#        gsizer.Add( bSizer3, 0, wx.EXPAND, 5 )
#
#        self.butsave.Bind(wx.EVT_BUTTON, self.OnSave)
#        self.butcsv.Bind(wx.EVT_BUTTON, self.OnCSV)
#        self.butdict.Bind(wx.EVT_BUTTON, self.OnDict)
#        self.butsave.SetBackgroundColour((14, 242, 14, 255))
#        self.butload.Bind(wx.EVT_BUTTON, self.OnLoad)
#        self.butaddcate.Bind(wx.EVT_BUTTON, self.OnAddCate)
#        self.OnStat()
#        self.SetSizer( gsizer )
#        self.Layout()
#
#    def __del__( self ):
#        pass
#
#
#    def OnLoad(self, event) :
#        if len(self.cate.cate['CATE']) != 0 :
#            message = wx.MessageDialog(self, _("Categories must be empty to load a categorization."), _("Information"), wx.OK|wx.ICON_WARNING)
#            message.ShowModal()
#            message.Destroy()
#            return
#        wildcard = "json|*.json|" \
#                   "All file|*.*"
#        dlg = wx.FileDialog(
#             self, message="Choose a file",
#             defaultDir=self.pathout.dirout,
#             defaultFile="",
#             wildcard=wildcard,
#             style=wx.FD_OPEN |
#                   wx.FD_CHANGE_DIR | wx.FD_FILE_MUST_EXIST |
#                   wx.FD_PREVIEW
#             )
#
#        if dlg.ShowModal() == wx.ID_OK:
#            paths = dlg.GetPaths()
#            path = paths[0]
#            self.cate.loadcate(path)
#            self.m_listCate.RefreshData(self.cate.getcate())
#            self.m_listToCate.RefreshData(self.cate.getwordstocate())
#        dlg.Destroy()
#
#    def OnSave(self, event) :
#        self.cate.save()
#        self.butsave.SetBackgroundColour((14, 242, 14, 255))
#
#    def OnCSV(self, event) :
#        wordscate = self.cate.getwordscate()
#        newtab = [['category%i' % i for i in range(1, len(self.tableau.selected_col)+1)]]
#        for line in self.tableau.select_col(self.tableau.selected_col):
#            newline = []
#            for word in line :
#                newline.append(wordscate.get(word,word))
#            newtab.append(newline)
#        with open(self.pathout['tableout.csv'], 'w', encoding='utf8') as f :
#            f.write('\n'.join(['\t'.join(line) for line in newtab]))
#        message = wx.MessageDialog(self, _("Export successful\n%s" % self.pathout['tableout.csv']), _("Information"), wx.OK|wx.ICON_INFORMATION)
#        message.ShowModal()
#        message.Destroy()
#
#    def OnDict(self, event):
#        with open(self.pathout['dictionnary.txt'], 'w', encoding='utf8') as f :
#            for categorie in self.cate.cate['CATE'] :
#                f.write(categorie + ': \t' + repr(self.cate.cate['CATE'][categorie][0]) + '\n')
#                for word in self.cate.cate['CATE'][categorie][1] :
#                    f.write('\t' + word + ': \t' + repr(self.cate.cate['CATE'][categorie][1][word]) + '\n')
#            for word in self.cate.cate['TOCATE'] :
#                f.write(word + ':\t' + repr(self.cate.cate['TOCATE'][word]) + '\n')
#        message = wx.MessageDialog(self, _("Export successful\n%s" % self.pathout['dictionnary.txt']), _("Information"), wx.OK|wx.ICON_INFORMATION)
#        message.ShowModal()
#        message.Destroy()
#
#    def OnStat(self) :
#        nbtocat, totocat, nbcate, totcate, lenwordincate = self.cate.makestat()
#        totwords = totocat + totcate
#        prtocat = repr(round((nbtocat/self.cate.lenwords) * 100 ,2))
#        prtotocat = repr(round((totocat/totwords) * 100, 2))
#        prcate = repr(round((totcate/totwords)*100, 2))
#        prwordincate = repr(round((lenwordincate/self.cate.lenwords)*100, 2))
#        self.stattxt.SetLabel(self.stat.format(nbtocat, prtocat, totocat, prtotocat, nbcate, lenwordincate, prwordincate, totcate, prcate))
#
#    def OnAddToTable(self) :
#        wordscate = self.cate.getwordscate()
#        newtab = [['category%i' % i for i in range(1, len(self.tableau.selected_col)+1)]]
#        for line in self.tableau.select_col(self.tableau.selected_col):
#            newline = []
#            for word in line :
#                newline.append(wordscate.get(word,word))
#            newtab.append(newline)
#
#    def OnAddCate(self, evt) :
#        print('add a category')
#        print(self.m_listCate.GetItemCount())
#        self.cate.addcatefromscratch()
#        self.m_listCate.dlist = self.cate.getcate()
#        self.m_listCate.RefreshData(self.m_listCate.dlist)
#        self.m_listCate.SetSelection(self.m_listCate.GetItemCount() - 1)



class ListPanel(wx.Panel) :
     def __init__(self, parent, gparent, List):
        wx.Panel.__init__(self, parent, style=wx.BORDER_SUNKEN)
        self.parent = parent
        self.gparent = gparent
        self.cate = gparent.cate
        gsizer =  wx.BoxSizer( wx.HORIZONTAL )
        self.list = List(self, dlist = gparent.cate, first = ['eff'])
        gsizer.Add(self.list, 5, wx.EXPAND, 5 )
        self.SetSizer( gsizer )
        self.Layout()


     def OnStat(self) :
        self.gparent.OnStat()

class ElCategorizator ( wx.Panel ):

    def __init__( self, parent, pathout, tableau, id = wx.ID_ANY, pos = wx.DefaultPosition, size = wx.Size( 500,300 ), style = wx.TAB_TRAVERSAL, name = wx.EmptyString ):
        wx.Panel.__init__ ( self, parent, id = id, pos = pos, size = size, style = style, name = name )
        self.pathout = pathout
        self.parent = parent
        self.tableau = tableau
        self.ira = wx.GetApp().GetTopWindow()

        self.cate = CategoDict(self.pathout)
        gsizer =  wx.BoxSizer( wx.VERTICAL )

        bSizer1 = wx.BoxSizer( wx.HORIZONTAL )
        splitter = MultiSplitterWindow(self, style=wx.SP_LIVE_UPDATE)
        self.splitter = splitter
        #sizer = wx.BoxSizer(wx.HORIZONTAL)
        #sizer.Add(splitter, 1, wx.EXPAND)
        #self.SetSizer(sizer)
        W,H = wx.GetTopLevelParent(self).GetSize()
        W = W - 200
        if W < 300 :
            W = 300
        splitsize = int(W/3)

        panelwords = ListPanel(splitter, self, ListForWords)
        splitter.AppendWindow(panelwords, splitsize)
        panelcate = ListPanel(splitter, self, ListForCate)
        splitter.AppendWindow(panelcate, splitsize)
        panelwordscate = ListPanel(splitter, self, ListForCateWords)
        splitter.AppendWindow(panelwordscate, splitsize)
        self.m_listToCate = panelwords.list
        self.m_listCate = panelcate.list
        self.m_listCateWords = panelwordscate.list


        bSizer2 = wx.BoxSizer( wx.HORIZONTAL )

        self.butsave = wx.Button( self, wx.ID_SAVE, _("Save"), wx.DefaultPosition, wx.DefaultSize, 0)
        bSizer2.Add( self.butsave, 0, wx.ALL, 5 )

        self.butcsv = wx.Button( self, wx.ID_ANY, _("Export Columns"), wx.DefaultPosition, wx.DefaultSize, 0 )
        bSizer2.Add( self.butcsv, 0, wx.ALL, 5 )

        self.butdict = wx.Button( self, wx.ID_ANY, _("Export dictonary"), wx.DefaultPosition, wx.DefaultSize, 0 )
        bSizer2.Add( self.butdict, 0, wx.ALL, 5 )

        self.butload = wx.Button( self, wx.ID_ANY, _("Load a categorization"), wx.DefaultPosition, wx.DefaultSize, 0 )
        bSizer2.Add( self.butload, 0, wx.ALL, 5 )

        self.butaddcate = wx.Button( self, wx.ID_ANY, _("Add a category"), wx.DefaultPosition, wx.DefaultSize, 0 )
        bSizer2.Add( self.butaddcate, 0, wx.ALL, 5 )


        bSizer3 = wx.BoxSizer( wx.HORIZONTAL )

        self.nbword = """Words : {:d} ({:d}) | """

        self.stat = """ Words to categorize : {:d} ({}%) - {:d} ({}%) -- Categories : {:d} - {:d} ({}%) - {:d} ({}%)"""
        for panel in [panelwords, panelcate, panelwordscate] :
            panel.m_listToCate = self.m_listToCate
            panel.m_listCate = self.m_listCate
            panel.m_listCateWords = self.m_listCateWords
            panel.butsave = self.butsave

#        nbtocat, totocat, nbcate, totcate = self.cate.makestat()
#        lenwords = self.cate.lenwords
#        totwords = totocat + totcate
#        prtocat = repr(nbtocat/lenwords)
#        prtotocat = repr(totocat/totwords)
#        prcate = repr(totcate/totwords)
        self.wordtxt = wx.StaticText(self, -1, "")
        bSizer3.Add( self.wordtxt, 0, wx.ALL, 5 )
        self.stattxt = wx.StaticText(self, -1, "")
        bSizer3.Add( self.stattxt, 0, wx.ALL, 5 )


        gsizer.Add( bSizer2, 0, wx.EXPAND, 5 )
        gsizer.Add( splitter, 2, wx.EXPAND, 5 )
        gsizer.Add( bSizer3, 0, wx.EXPAND, 5 )

        self.butsave.Bind(wx.EVT_BUTTON, self.OnSave)
        self.butcsv.Bind(wx.EVT_BUTTON, self.OnCSV)
        self.butdict.Bind(wx.EVT_BUTTON, self.OnDict)
        self.butsave.SetBackgroundColour((14, 242, 14, 255))
        self.butload.Bind(wx.EVT_BUTTON, self.OnLoad)
        self.butaddcate.Bind(wx.EVT_BUTTON, self.OnAddCate)
        self.OnStat()
        self.SetSizer( gsizer )
        self.Layout()



    def __del__( self ):
        pass

    def OnLoad(self, event) :
        if len(self.cate.cate['CATE']) != 0 :
            message = wx.MessageDialog(self, _("Categories must be empty to load a categorization."), _("Information"), wx.OK|wx.ICON_WARNING)
            message.ShowModal()
            message.Destroy()
            return
        wildcard = "json|*.json|" \
                   "All file|*.*"
        dlg = wx.FileDialog(
             self, message="Choose a file",
             defaultDir=self.pathout.dirout,
             defaultFile="",
             wildcard=wildcard,
             style=wx.FD_OPEN |
                   wx.FD_CHANGE_DIR | wx.FD_FILE_MUST_EXIST |
                   wx.FD_PREVIEW
             )

        if dlg.ShowModal() == wx.ID_OK:
            paths = dlg.GetPaths()
            path = paths[0]
            self.cate.loadcate(path)
            self.m_listCate.RefreshData(self.cate.getcate())
            self.m_listToCate.RefreshData(self.cate.getwordstocate())
        dlg.Destroy()

    def OnSave(self, event) :
        self.cate.save()
        self.butsave.SetBackgroundColour((14, 242, 14, 255))
        self.parent.Refresh()


    def OnCSV(self, event) :
        wordscate = self.cate.getwordscate()
        newtab = [['category%i' % i for i in range(1, len(self.tableau.selected_col)+1)]]
        for line in self.tableau.select_col(self.tableau.selected_col):
            newline = []
            for word in line :
                newline.append(wordscate.get(word,word))
            newtab.append(newline)
        with open(self.pathout['tableout.csv'], 'w', encoding='utf8') as f :
            f.write('\n'.join(['\t'.join(line) for line in newtab]))
        message = wx.MessageDialog(self, _("Export successful\n%s" % self.pathout['tableout.csv']), _("Information"), wx.OK|wx.ICON_INFORMATION)
        message.ShowModal()
        message.Destroy()

    def OnDict(self, event):
        with open(self.pathout['dictionnary.txt'], 'w', encoding='utf8') as f :
            for categorie in self.cate.cate['CATE'] :
                f.write(categorie + ': \t' + repr(self.cate.cate['CATE'][categorie][0]) + '\n')
                for word in self.cate.cate['CATE'][categorie][1] :
                    f.write('\t' + word + ': \t' + repr(self.cate.cate['CATE'][categorie][1][word]) + '\n')
            for word in self.cate.cate['TOCATE'] :
                f.write(word + ':\t' + repr(self.cate.cate['TOCATE'][word]) + '\n')
        message = wx.MessageDialog(self, _("Export successful\n%s" % self.pathout['dictionnary.txt']), _("Information"), wx.OK|wx.ICON_INFORMATION)
        message.ShowModal()
        message.Destroy()

    def OnStat(self) :
        nbtocat, totocat, nbcate, totcate, lenwordincate = self.cate.makestat()
        totwords = totocat + totcate
        prtocat = repr(round((nbtocat/self.cate.lenwords) * 100 ,2))
        prtotocat = repr(round((totocat/totwords) * 100, 2))
        prcate = repr(round((totcate/totwords)*100, 2))
        prwordincate = repr(round((lenwordincate/self.cate.lenwords)*100, 2))
        self.stattxt.SetLabel(self.stat.format(nbtocat, prtocat, totocat, prtotocat, nbcate, lenwordincate, prwordincate, totcate, prcate))


    def OnAddToTable(self) :
        wordscate = self.cate.getwordscate()
        newtab = [['category%i' % i for i in range(1, len(self.tableau.selected_col)+1)]]
        for line in self.tableau.select_col(self.tableau.selected_col):
            newline = []
            for word in line :
                newline.append(wordscate.get(word,word))
            newtab.append(newline)

    def OnAddCate(self, evt) :
        self.cate.addcatefromscratch()
        self.m_listCate.dlist = self.cate.getcate()
        self.m_listCate.RefreshData(self.m_listCate.dlist)
        self.m_listCate.SetSelection(self.m_listCate.GetItemCount() - 1)


class ListForCate(wx.ListCtrl, listmix.ListCtrlAutoWidthMixin, listmix.ColumnSorterMixin):

    def __init__(self, parent, dlist = {}, first = [], usefirst = False, menu = True):
        wx.ListCtrl.__init__( self, parent, -1, style=wx.LC_REPORT|wx.LC_VIRTUAL|wx.LC_HRULES|wx.LC_VRULES|wx.LC_EDIT_LABELS|wx.LC_SINGLE_SEL)
        self.parent=parent
        self.cate = self.parent.cate
        self.dlist= self.cate.getcate()
        self.first = first
        self.il = wx.ImageList(20, 20)
        a={"sm_up":"GO_UP","sm_dn":"GO_DOWN","w_idx":"WARNING","e_idx":"ERROR","i_idx":"QUESTION", "p_idx":"PLUS"}
        for k,v in list(a.items()):
            s="self.%s= self.il.Add(wx.ArtProvider.GetBitmap(wx.ART_%s,wx.ART_TOOLBAR,(20,20)))" % (k,v)
            exec(s)
        self.SetImageList(self.il, wx.IMAGE_LIST_SMALL)
        self.attr1 = wx.ItemAttr()
        self.attr1.SetBackgroundColour((230, 230, 230))
        self.attr2 = wx.ItemAttr()
        self.attr2.SetBackgroundColour("light blue")
        #self.attrselected = wx.ListItemAttr()
        #self.attrselected.SetBackgroundColour("red")
        self.SetListFont()
        self.selected = {}
        i = 0
        for name in ['Categories'] + self.first :
            self.InsertColumn(i,name,wx.LIST_FORMAT_LEFT)
            i += 1
        self.itemDataMap = self.dlist
        self.itemIndexMap = list(self.dlist.keys())
        self.SetItemCount(len(self.dlist))
        listmix.ListCtrlAutoWidthMixin.__init__(self)
        listmix.ColumnSorterMixin.__init__(self, len(self.first) + 1)

        #self.SortListItems(1, False)
        self.SetColumnWidth(0, 300)
        self.SetColumnWidth(1, wx.LIST_AUTOSIZE)

        self.Bind(wx.EVT_LIST_BEGIN_DRAG, self.StartDrag)
        self.Bind(wx.EVT_LIST_ITEM_FOCUSED, self.ShowWords)
        self.Bind(wx.EVT_LIST_ITEM_ACTIVATED, self.OnBeginEdit)
        self.Bind(wx.EVT_LIST_END_LABEL_EDIT, self.OnEndEdit)
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected)
        self.Bind(wx.EVT_LIST_COL_CLICK, self.OnSortColumn)

        if self.GetItemCount() != 0 :
            #self.SetItemState(0, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)
            self.Select(0, on=1)

        dt = MyListDropCate(self)
        self.SetDropTarget(dt)

    def OnSortColumn(self, evt) :
        #print(self.currentItem)
        evt.Skip()

    def SetListFont(self) :
        self.SetFont(wx.Font(16, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL))

    def OnGetItemImage(self, item):
        return self.p_idx

    def OnBeginEdit(self, event) :
        self.EditLabel(event.GetIndex())
        event.Skip()

    def OnEndEdit(self, event) :
        newlabel = event.GetLabel()
        idx = event.GetIndex()
        oldlabel = self.GetItemText(idx)
        if newlabel not in self.cate.cate['CATE'] :
            self.cate.cate['CATE'][newlabel] = self.cate.cate['CATE'][oldlabel]
            del(self.cate.cate['CATE'][oldlabel])
        self.RefreshData(self.cate.getcate())

    def ShowWords(self, event) :
        index = event.GetIndex()
        try :
            data = self.cate.getcatewords(self.GetItemText(index))
            self.parent.m_listCateWords.RefreshData(data)
            self.parent.m_listCateWords.SetSelection(0)
        except :
            pass
        event.Skip()

    def RefreshData(self, data):
        try :
            item = self.currentItem
        except :
            item = 0
        self.itemDataMap = data
        self.itemIndexMap = list(data.keys())
        self.SetItemCount(len(data))
        order = self._colSortFlag[self._col]
        if len(data) :
            self.SortListItems(self._col, order)
        #self.SetColumnWidth(0, wx.LIST_AUTOSIZE)
        #self.SetColumnWidth(1, wx.LIST_AUTOSIZE)
        #self.SetColumnWidth(0,300)
        self.parent.OnStat()
        self.Refresh()
        try :
            self.SetSelection(item)
            self.Focus(item)
        except :
            pass

    def GetListCtrl(self):
        return self

    def GetSortImages(self):
        return (self.sm_dn, self.sm_up)

    def SortItems(self, sorter=None):
        #print(self.currentItem)
        try :
            select = self.currentItem
            word = self.GetItemData(select)[0]
        except:
            print("no item selected")
        #Exception as e: print('execption SortItems ListForCate :',e)
        listTemp = sorted(self.itemDataMap.items(),
           key=lambda x:x[1][self._col], reverse= (self._colSortFlag[self._col]!=True))
        dlist = dict([[line[0],line[1]] for line in listTemp])
        self.itemDataMap = dlist
        self.itemIndexMap = list(dlist.keys())
        self.Refresh() # redraw the list
        try :
            formes = [self.getColumnText(i, 0) for i in range(self.GetItemCount())]
            idx = [i for i, val in enumerate(formes) if val == word][0]
            self.SetSelection(idx)
            self.Focus(idx)
        except Exception as e: print('execption SortItems2 : ',e)

    def OnGetItemText(self, item, col):
        index=self.itemIndexMap[item]
        s = self.itemDataMap[index][col]
        if isinstance(s, (int, float)):
            return str(s)
        else:
            return s #modification pour python 3

    def OnGetItemAttr(self, item):
#        if self.IsSelected(index) == True :
#            print('selected', index)
        index=self.itemIndexMap[item]
        if item % 2 :
           return self.attr1
        else :
           return self.attr2

    def getselectedwords(self) :
        words = [self.getColumnText(self.GetFirstSelected(), 0)]
        last = self.GetFirstSelected()
        while self.GetNextSelected(last) != -1:
            last = self.GetNextSelected(last)
            words.append(self.getColumnText(last, 0))
        return words

    def GetString(self):
        return self.getselectedwords()[0]

    def GetSelections(self):
        return self.getselectedwords()

    def getColumnText(self, index, col):
        try:
            item = self.GetItem(index, col)
            return item.GetText()
        except:
            message = wx.MessageDialog(self, _("You have to select a category first"), _("Information"), wx.OK|wx.ICON_INFORMATION)
            message.ShowModal()
            message.Destroy()

    def GetItemData(self, item) :
        index=self.itemIndexMap[item]
        s = self.itemDataMap[index]
        return s

    def OnItemSelected(self, event):
        self.currentItem = event.GetIndex() #event.m_itemIndex
        categorie = self.GetItemData(self.currentItem)[0]
        self.parent.m_listCateWords.RefreshData(self.cate.getcatewords(categorie))
        event.Skip()

    def SetSelection(self, index) :
        for i in range(0, self.GetItemCount(), 1) :
            self.Select(i, on=0)
        self.Select(index, on=1)

    def GetItemInfo(self, idx):
        """
        Collect all relevant data of a listitem, and put it in a list.
        """

        l = []
        l.append(idx) # We need the original index, so it is easier to eventualy delete it.
        l.append(self.GetItemData(idx)) # Itemdata.
        l.append(self.GetItemText(idx)) # Text first column.
        for i in range(1, self.GetColumnCount()): # Possible extra columns.
            l.append(self.GetItem(idx, i).GetText())
        l.append('cate')
        return l


    def StartDrag(self, event):
        """
        Put together a data object for drag-and-drop _from_ this list.
        """

        l = []
        idx = -1
        while True: # Find all the selected items and put them in a list.
            idx = self.GetNextItem(idx, wx.LIST_NEXT_ALL, wx.LIST_STATE_SELECTED)
            if idx == -1:
                break
            l.append(self.GetItemInfo(idx))

        # Pickle the items list.
        itemdata = pickle.dumps(l, 1)
        # Create our own data format and use it
        # in a Custom data object.
        ldata = wx.CustomDataObject("ListCtrlItems")
        ldata.SetData(itemdata)
        # Now make a data object for the  item list.
        data = wx.DataObjectComposite()
        data.Add(ldata)

        # Create drop source and begin drag-and-drop.
        dropSource = wx.DropSource(self)
        dropSource.SetData(data)
        res = dropSource.DoDragDrop(flags=wx.Drag_DefaultMove)

        # If move, we want to remove the item from this list.
        if res == wx.DragMove and l[0][-1] != 'cate' :
            # It's possible we are dragging/dropping from this list to this list.
            # In which case, the index we are removing may have changed...

            # Find correct position.
            l.reverse() # Delete all the items, starting with the last item.
            for i in l:
                pos = self.FindItem(i[0], i[2])
                self.DeleteItem(pos)


    def Insert(self, x, y, seq):
        """
        Insert text at given x, y coordinates --- used with drag-and-drop.
        """

        # Find insertion point.
        index, flags = self.HitTest((x, y))

        if index == wx.NOT_FOUND: # Not clicked on an item.
            if flags & (wx.LIST_HITTEST_NOWHERE|wx.LIST_HITTEST_ABOVE|wx.LIST_HITTEST_BELOW): # Empty list or below last item.
                index = self.GetItemCount() # Append to end of list.
            elif self.GetItemCount() > 0:
                if y <= self.GetItemRect(0).y: # Clicked just above first item.
                    index = -1 # Append to top of list.
                else:
                    index = self.GetItemCount() + 1 # Append to end of list.
        else: # Clicked on an item.
            # Get bounding rectangle for the item the user is dropping over.
            rect = self.GetItemRect(index)

            # If the user is dropping into the lower half of the rect,
            # we want to insert _after_ this item.
            # Correct for the fact that there may be a heading involved.
            #if y > rect.y - self.GetItemRect(0).y + rect.height/2:
            #    index += 1
        #print('Insert de ListForCate', index, flags)
        word, eff = seq[0][1]
        if seq[0][-1] == 'words' :
            if index < self.GetItemCount() and index != -1 :
                for val in seq :
                    word, eff = val[1]
                    self.cate.addwordincate(self.GetItemData(index)[0], word, eff)
            else :
                index = self.GetItemCount()
                if self.cate.addcatefromwordtocate(word, eff) :
                    pass
                else :
                    dial = wx.MessageDialog(self, "This category name is already used", style=wx.OK|wx.CENTRE)
                    dial.ShowModal()
                    dial.Destroy()
                    return
            self.dlist = self.cate.getcate()
            self.RefreshData(self.dlist)
            self.parent.m_listToCate.RefreshData(self.cate.getwordstocate())
            #self.parent.m_listCate.SetSelection(index)
            self.parent.m_listCateWords.RefreshData(self.parent.cate.getcatewords(self.GetItemData(index)[0]))
            for i in range(0, self.GetItemCount(), 1):
                self.Select(i, on=0)
            self.Select(index, on=1)
            self.parent.butsave.SetBackgroundColour((255,0,0,255))
            self.parent.Refresh()
        if seq[0][-1] == 'catewords' :
            if index < self.GetItemCount() and index != -1 :
                cible = self.GetItemData(index)[0]
                for val in seq :
                    word, eff = val[1]
                    if word not in self.cate.cate['CATE'][self.GetItemData(index)[0]][1] :
                        self.cate.changewordcate(self.GetItemData(index)[0], word, eff)
                        self.parent.butsave.SetBackgroundColour((255,0,0,255))
                self.parent.Refresh()
            else :
                index = self.GetItemCount()
                if self.cate.addcatefromwordcate(word, eff) :
                    self.parent.butsave.SetBackgroundColour((255,0,0,255))
                    self.parent.Refresh()
                else :
                    dial = wx.MessageDialog(self, "This category name is already used", style=wx.OK|wx.CENTRE)
                    dial.ShowModal()
                    dial.Destroy()
                    return
                #self.cate.addwordincate(self.GetItemData(index)[0], word, eff)
            self.dlist = self.cate.getcate()
            self.RefreshData(self.dlist)
            self.parent.m_listToCate.RefreshData(self.cate.getwordstocate())
            #self.parent.m_listCateWords.RefreshData(self.parent.cate.getcatewords(self.GetItemData(index)[0]))
            self.parent.m_listCate.SetSelection(index)
            self.parent.m_listCateWords.RefreshData(self.parent.cate.getcatewords(self.GetItemData(index)[0]))
            #self.SetSelection(index)



#        for i in seq: # Insert the item data.
#            idx = self.InsertItem(index, i[2])
#            self.SetItemData(idx, i[1])
#            for j in range(1, self.GetColumnCount()):
#                try: # Target list can have more columns than source.
#                    self.SetItem(idx, j, i[2+j])
#                except:
#                    pass # Ignore the extra columns.
#            index += 1

class ListForWords(ListForCate) :
    def __init__(self, parent, dlist = {}, first = []):
        wx.ListCtrl.__init__( self, parent, -1, style=wx.LC_REPORT|wx.LC_VIRTUAL|wx.LC_HRULES|wx.LC_VRULES)
        self.parent=parent
        self.cate = self.parent.cate
        self.dlist= self.cate.getwordstocate()
        self.first = first
        self.il = wx.ImageList(16, 16)
        a={"sm_up":"GO_UP","sm_dn":"GO_DOWN","w_idx":"WARNING","e_idx":"ERROR","i_idx":"QUESTION"}
        for k,v in list(a.items()):
            s="self.%s= self.il.Add(wx.ArtProvider.GetBitmap(wx.ART_%s,wx.ART_TOOLBAR,(16,16)))" % (k,v)
            exec(s)
        self.SetImageList(self.il, wx.IMAGE_LIST_SMALL)
        self.attr1 = wx.ItemAttr()
        self.attr1.SetBackgroundColour((230, 230, 230))
        self.attr2 = wx.ItemAttr()
        self.attr2.SetBackgroundColour("light blue")
        #self.attrselected = wx.ListItemAttr()
        #self.attrselected.SetBackgroundColour("red")
        self.SetListFont()
        self.selected = {}
        i = 0
        for name in ['To categorize'] + self.first :
            self.InsertColumn(i,name,wx.LIST_FORMAT_LEFT)
            i += 1
        self.itemDataMap = self.dlist
        self.itemIndexMap = list(self.dlist.keys())
        self.SetItemCount(len(self.dlist))
        listmix.ListCtrlAutoWidthMixin.__init__(self)
        listmix.ColumnSorterMixin.__init__(self, len(self.first) + 1)
        self.SetColumnWidth(0, 400)
        self.SetColumnWidth(1, wx.LIST_AUTOSIZE)

        self.SortListItems(1, False)

        self.Bind(wx.EVT_LIST_BEGIN_DRAG, self.StartDrag)
        self.Bind(wx.EVT_LIST_ITEM_ACTIVATED, self.OnDClick)
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected)

        dt = MyListDropCate(self)
        self.SetDropTarget(dt)

    def OnDClick(self, event) :
        idx = event.GetIndex()
        event.Skip()

    def OnItemSelected(self, event):
        self.currentItem = event.GetIndex() #event.m_itemIndex
        event.Skip()

    def OnGetItemImage(self, item):
        return self.i_idx

    def GetItemInfo(self, idx):
        """
        Collect all relevant data of a listitem, and put it in a list.
        """

        l = []
        l.append(idx) # We need the original index, so it is easier to eventualy delete it.
        l.append(self.GetItemData(idx)) # Itemdata.
        l.append(self.GetItemText(idx)) # Text first column.
        for i in range(1, self.GetColumnCount()): # Possible extra columns.
            l.append(self.GetItem(idx, i).GetText())
        l.append('words')
        return l


    def StartDrag(self, event):
        """
        Put together a data object for drag-and-drop _from_ this list.
        """

        l = []
        idx = -1
        while True: # Find all the selected items and put them in a list.
            idx = self.GetNextItem(idx, wx.LIST_NEXT_ALL, wx.LIST_STATE_SELECTED)
            if idx == -1:
                break
            l.append(self.GetItemInfo(idx))

        # Pickle the items list.
        itemdata = pickle.dumps(l, 1)
        # Create our own data format and use it
        # in a Custom data object.
        ldata = wx.CustomDataObject("ListCtrlItems")
        ldata.SetData(itemdata)
        # Now make a data object for the  item list.
        data = wx.DataObjectComposite()
        data.Add(ldata)

        # Create drop source and begin drag-and-drop.
        dropSource = wx.DropSource(self)
        dropSource.SetData(data)
        res = dropSource.DoDragDrop(flags=wx.Drag_DefaultMove)


        # If move, we want to remove the item from this list.
        if res == wx.DragMove and l[0][-1] != 'words':
            # It's possible we are dragging/dropping from this list to this list.
            # In which case, the index we are removing may have changed...

            # Find correct position.
            l.reverse() # Delete all the items, starting with the last item.
            for i in l:
                pos = self.FindItem(i[0], i[2])
                #print('detruit : ',pos)
                self.DeleteItem(pos)


    def Insert(self, x, y, seq):
        """
        Insert text at given x, y coordinates --- used with drag-and-drop.
        """

        # Find insertion point.
        index, flags = self.HitTest((x, y))

        if index == wx.NOT_FOUND: # Not clicked on an item.
            if flags & (wx.LIST_HITTEST_NOWHERE|wx.LIST_HITTEST_ABOVE|wx.LIST_HITTEST_BELOW): # Empty list or below last item.
                index = self.GetItemCount() # Append to end of list.
            elif self.GetItemCount() > 0:
                if y <= self.GetItemRect(0).y: # Clicked just above first item.
                    index = 0 # Append to top of list.
                else:
                    index = self.GetItemCount() + 1 # Append to end of list.
        else: # Clicked on an item.
            # Get bounding rectangle for the item the user is dropping over.
            rect = self.GetItemRect(index)

            # If the user is dropping into the lower half of the rect,
            # we want to insert _after_ this item.
            # Correct for the fact that there may be a heading involved.
            if y > rect.y - self.GetItemRect(0).y + rect.height/2:
                index += 1
        word, eff = seq[0][1]
        if seq[0][-1] == 'catewords' :
            for val in seq :
                word, eff = val[1]
                categorie = self.cate.findcatefromword(word)
                self.cate.addwordinwords(categorie, word, eff)
            self.RefreshData(self.cate.getwordstocate())
            self.parent.m_listCate.RefreshData(self.cate.getcate())
            self.parent.m_listCateWords.RefreshData(self.cate.getcatewords(categorie))
            self.parent.butsave.SetBackgroundColour((255,0,0,255))
            #self.parent.Refresh()
        elif seq[0][-1] == 'cate' :
            categorie = seq[0][1][0]
            self.cate.delcate(categorie)
            self.RefreshData(self.cate.getwordstocate())
            self.parent.m_listCate.RefreshData(self.cate.getcate())
            if self.parent.m_listCate.GetItemCount() != 0 :
                self.parent.m_listCate.SetSelection(0)
                self.parent.m_listCateWords.RefreshData(self.cate.getcatewords(self.parent.m_listCate.GetItemText(0)))
            else :
                self.parent.m_listCateWords.RefreshData({})
            self.parent.butsave.SetBackgroundColour((255,0,0,255))
            #self.parent.Refresh()



class ListForCateWords(ListForCate) :
    def __init__(self, parent, dlist = {}, first = []):
        wx.ListCtrl.__init__( self, parent, -1, style=wx.LC_REPORT|wx.LC_VIRTUAL|wx.LC_HRULES|wx.LC_VRULES)
        self.parent=parent
        self.cate = self.parent.cate
        self.dlist= {}
        self.first = first
        self.il = wx.ImageList(16, 16)
        a={"sm_up":"GO_UP","sm_dn":"GO_DOWN","p_idx":"TIP","e_idx":"ERROR","i_idx":"QUESTION"}
        for k,v in list(a.items()):
            s="self.%s= self.il.Add(wx.ArtProvider.GetBitmap(wx.ART_%s,wx.ART_TOOLBAR,(16,16)))" % (k,v)
            exec(s)
        self.SetImageList(self.il, wx.IMAGE_LIST_SMALL)
        self.attr1 = wx.ItemAttr()
        self.attr1.SetBackgroundColour((230, 230, 230))
        self.attr2 = wx.ItemAttr()
        self.attr2.SetBackgroundColour("light blue")
        #self.attrselected = wx.ListItemAttr()
        #self.attrselected.SetBackgroundColour("red")
        self.SetListFont()
        self.selected = {}
        i = 0
        for name in ['Contents'] + self.first :
            self.InsertColumn(i,name,wx.LIST_FORMAT_LEFT)
            i += 1
        self.itemDataMap = self.dlist
        self.itemIndexMap = list(self.dlist.keys())
        self.SetItemCount(len(self.dlist))
        listmix.ListCtrlAutoWidthMixin.__init__(self)
        listmix.ColumnSorterMixin.__init__(self, len(self.first) + 1)
        self.SetColumnWidth(0, 300)
        self.SetColumnWidth(1, wx.LIST_AUTOSIZE)

        self.SortListItems(1, False)

        self.Bind(wx.EVT_LIST_BEGIN_DRAG, self.StartDrag)
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected)

        dt = MyListDropCate(self)
        self.SetDropTarget(dt)

    def OnItemSelected(self, event):
        self.currentItem = event.GetIndex() #event.m_itemIndex
        event.Skip()


    def GetItemInfo(self, idx):
        """
        Collect all relevant data of a listitem, and put it in a list.
        """

        l = []
        l.append(idx) # We need the original index, so it is easier to eventualy delete it.
        l.append(self.GetItemData(idx)) # Itemdata.
        l.append(self.GetItemText(idx)) # Text first column.
        for i in range(1, self.GetColumnCount()): # Possible extra columns.
            l.append(self.GetItem(idx, i).GetText())
        l.append('catewords')
        return l


    def StartDrag(self, event):
        """
        Put together a data object for drag-and-drop _from_ this list.
        """

        l = []
        idx = -1
        while True: # Find all the selected items and put them in a list.
            idx = self.GetNextItem(idx, wx.LIST_NEXT_ALL, wx.LIST_STATE_SELECTED)
            if idx == -1:
                break
            l.append(self.GetItemInfo(idx))

        # Pickle the items list.
        itemdata = pickle.dumps(l, 1)
        # Create our own data format and use it
        # in a Custom data object.
        ldata = wx.CustomDataObject("ListCtrlItems")
        ldata.SetData(itemdata)
        # Now make a data object for the  item list.
        data = wx.DataObjectComposite()
        data.Add(ldata)

        # Create drop source and begin drag-and-drop.
        dropSource = wx.DropSource(self)
        dropSource.SetData(data)
        res = dropSource.DoDragDrop(flags=wx.Drag_DefaultMove)
        #print('current')
        #print(self.parent.m_listCate.currentItem)

        # If move, we want to remove the item from this list.
        #if res == wx.DragMove:
        #    # It's possible we are dragging/dropping from this list to this list.
        #    # In which case, the index we are removing may have changed...

        #    # Find correct position.
        #    l.reverse() # Delete all the items, starting with the last item.
        #    for i in l:
        #        pos = self.FindItem(i[0], i[2])
        #        self.DeleteItem(pos)


    def Insert(self, x, y, seq):
        """
        Insert text at given x, y coordinates --- used with drag-and-drop.
        """
        pass
        # Find insertion point.
        index, flags = self.HitTest((x, y))
#
#        if index == wx.NOT_FOUND: # Not clicked on an item.
#            if flags & (wx.LIST_HITTEST_NOWHERE|wx.LIST_HITTEST_ABOVE|wx.LIST_HITTEST_BELOW): # Empty list or below last item.
#                index = self.GetItemCount() # Append to end of list.
#            elif self.GetItemCount() > 0:
#                if y <= self.GetItemRect(0).y: # Clicked just above first item.
#                    index = 0 # Append to top of list.
#                else:
#                    index = self.GetItemCount() + 1 # Append to end of list.
#        else: # Clicked on an item.
#            # Get bounding rectangle for the item the user is dropping over.
#            rect = self.GetItemRect(index)
#
#            # If the user is dropping into the lower half of the rect,
#            # we want to insert _after_ this item.
#            # Correct for the fact that there may be a heading involved.
#            if y > rect.y - self.GetItemRect(0).y + rect.height/2:
#                index += 1
        #print('Insert de ListForCateWords', index,flags)
        if self.parent.m_listCate.GetFirstSelected() != -1 :
            categorie = self.parent.m_listCate.getColumnText(self.parent.m_listCate.GetFirstSelected(),0)
        #if categorie is not None :
            if seq[0][-1] == 'words' :
                for val in seq :
                    word, eff = val[1]
                    self.cate.addwordincate(categorie, word, eff)
                self.dlist = self.cate.getwordstocate()
                self.RefreshData(self.cate.getcatewords(categorie))
                self.parent.m_listCate.RefreshData(self.cate.getcate())
                self.parent.m_listToCate.RefreshData(self.dlist)
                self.parent.butsave.SetBackgroundColour((255,0,0,255))
                #self.parent.Refresh()
        else :
            message = wx.MessageDialog(self, _("You have to select a category first"), _("Information"), wx.OK|wx.ICON_INFORMATION)
            message.ShowModal()
            message.Destroy()

class MyListDropCate(wx.DropTarget):
    """
    Drop target for simple lists.
    """
    def __init__(self, source):
        """
        Arguments:
        source: source listctrl.
        """
        wx.DropTarget.__init__(self)

        #------------

        self.dv = source

        #------------

        # Specify the type of data we will accept.
        self.data = wx.CustomDataObject("ListCtrlItems")
        self.SetDataObject(self.data)

    #-----------------------------------------------------------------------

    # Called when OnDrop returns True.
    # We need to get the data and do something with it.
    def OnData(self, x, y, d):
        """
        ...
        """

        # Copy the data from the drag source to our data object.
        if self.GetData():
            # Convert it back to a list and give it to the viewer.
            ldata = self.data.GetData()
            l = pickle.loads(ldata)
            self.dv.Insert(x, y, l)

        # What is returned signals the source what to do
        # with the original data (move, copy, etc.)  In this
        # case we just return the suggested value given to us.
        return d
