# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules python
#------------------------------------
import xml.dom.minidom
import os
import codecs
import re

#------------------------------------
# import des modules wx
#------------------------------------
import wx
import wx.lib.filebrowsebutton as filebrowse

#------------------------------------
# import des fichiers du projet
#------------------------------------
from parse_factiva_mail import ParseFactivaMail
from parse_factiva_txt import ParseFactivaPaste
from parse_europress import ParseEuropress
from import_txm import TXM2IRA
from functions import BugReport


def ParseDocument(filename) :
    with codecs.open(filename, 'r', 'utf-8') as f :
        content = f.read()
    content = content.replace('<hlt>', ' ').replace('</hlt>', ' ')
    dom = xml.dom.minidom.parseString(content)
    result = []
    articles = dom.getElementsByTagName("article")
    for article in articles :
        headline = article.getElementsByTagName("headline")
        if headline != [] :
            para_headline = headline[0].getElementsByTagName("paragraph")
            val_headline = [val.firstChild.nodeValue.replace('\n', ' ') for val in para_headline]
        else :
            val_headline = []
        leadParagraph = article.getElementsByTagName("leadParagraph")
        if leadParagraph != [] :
            para_leadParagraph = leadParagraph[0].getElementsByTagName("paragraph")
            val_leadParagraph = [val.firstChild.nodeValue.replace('\n', ' ') for val in para_leadParagraph]
        else :
            val_leadParagraph = []
        publicationDate = article.getElementsByTagName("publicationDate")
        if publicationDate != [] :
            para_publicationDate = publicationDate[0].getElementsByTagName("date")
            if para_publicationDate == [] :
                para_publicationDate = publicationDate[0].getElementsByTagName("dateTime")
            val_publicationDate = [val.firstChild.nodeValue.replace('\n', ' ') for val in para_publicationDate]
        else :
            val_publicationDate = []
        sourceName = article.getElementsByTagName("sourceName")
        if sourceName != [] :
            val_sourceName = sourceName[0].firstChild.nodeValue.replace('\n', ' ')
        else :
            val_sourceName = 'INCONNU'
        tailParagraphs = article.getElementsByTagName("tailParagraphs")
        if tailParagraphs != [] :
            para_tailParagraphs = tailParagraphs[0].getElementsByTagName("paragraph")
            val_tailParagraphs = [val.firstChild.nodeValue.replace('\n', ' ') for val in para_tailParagraphs]
        else :
            val_tailParagraphs = []
        inter = [' '.join(val_headline), val_sourceName,' '.join(val_publicationDate), ' '.join(val_leadParagraph), ' '.join(val_tailParagraphs)]
        inter = [re.sub(r'[ "\n\r]+', ' ',  val).replace('"',' ').replace('\n', ' ').replace('\r', ' ')  for val in inter]
        #inter = ['"' + val +'"' for val in inter]
        result.append(inter)
    return result

def getcorpus_from_xml(xmldir, corpus_out):
    files = os.listdir(xmldir)
    files = [os.path.join(xmldir,f) for f in files if os.path.splitext(f)[1] == '.xml']
    if len(files) == 0 :
        return 'nofile'
    fileout = codecs.open(corpus_out, 'w', 'utf-8')
    for f in files :
        rs = ParseDocument(f)
        #dates = [row[2].split('-') for row in rs]
        #dates = [[date[0],date[1],date[2].split('T')[0]] for date in dates]
        #txt = '\n'.join(['\n'.join([' '.join([u'****', '*%s' % row[1].replace(' ','_').replace('\'','_'), '*%s' % row[2].replace('-','_')]), row[3], row[4]]) for row in rs])
        #avec la date decompose
        txt = '\n'.join(['\n'.join([' '.join(['****', '*s_%s' % row[1].replace(' ','').replace('\'',''), '*annee_%s' % row[2].split('-')[0], '*mois_%s' % row[2].split('-')[1], '*jour_%s' % row[2].split('-')[2].split('T')[0]]), row[3], row[4]]) for row in rs])
        fileout.write(txt+'\n\n')
    fileout.close()
    return 'ok'


class PrefImport(wx.Dialog):

    def __init__(self, parent, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE, methode = 'mail'):
        wx.Dialog.__init__(self)                       # 1
        self.SetExtraStyle(wx.DIALOG_EX_CONTEXTHELP)    # 2
        self.Create(parent, -1, '')                 # 3
        self.methode = methode
        if methode in ['xml', 'txm'] :
            txt = _('Select a directory of xml files')
        elif methode == 'euro' :
            txt = _('Select a directory of html files')
        elif methode == 'dmi' :
            txt = _('Select a csv file')
        else :
            txt = _('Select a directory of txt files')
        self.parent = parent
        self.txt1 = wx.StaticText(self, -1, txt)
        if methode != 'dmi' :
            self.dbb = filebrowse.DirBrowseButton(self, -1, size=(450, -1), changeCallback = self.fbbCallback)
        else :
            self.dbb = filebrowse.FileBrowseButton(self, -1, size=(450, -1), fileMode = 2, changeCallback = self.fbbCallback)
        self.dbb.SetLabel("")
        self.txt2 = wx.StaticText(self, -1, _('Output file'))
        self.fbb = filebrowse.FileBrowseButton(self, -1, size=(450, -1), fileMode = 2)
        self.fbb.SetLabel("")
        self.btnsizer = wx.StdDialogButtonSizer()
        btn_ok = wx.Button(self, wx.ID_OK)
        btn = wx.Button(self, wx.ID_CANCEL)
        self.btnsizer.AddButton(btn_ok)
        self.btnsizer.AddButton(btn)
        self.btnsizer.Realize()
        self.Bind(wx.EVT_BUTTON, self.checkfile, btn_ok)
        #self.SetButtonSizer(self.CreateStdDialogButtonSizer(wx.OK | wx.CANCEL))
        self.Bind(wx.EVT_BUTTON, self.checkfile)
        self. __do_layout()
        #self.Fit()
        self.SetMinSize(self.GetSize())

    def __do_layout(self):
        sizer = wx.BoxSizer(wx.VERTICAL)
        grid_sizer_1 = wx.BoxSizer(wx.HORIZONTAL)
        grid_sizer_2 = wx.BoxSizer(wx.HORIZONTAL)
        grid_sizer_1.Add(self.txt1, 0, wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL, 0)
        grid_sizer_1.Add(self.dbb, 2, wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL, 0)
        grid_sizer_2.Add(self.txt2, 0, wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL, 0)
        grid_sizer_2.Add(self.fbb, 2, wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL, 0)
        sizer.Add(grid_sizer_1, 0,  wx.EXPAND, 0)
        sizer.Add(grid_sizer_2, 0,  wx.EXPAND, 0)
        sizer.Add(self.btnsizer, 0,  wx.EXPAND, 0)
        self.SetSizer(sizer)
        sizer.Fit(self)
        self.Layout()

    def fbbCallback(self, evt):
        if self.fbb.GetValue() == "" :
            if self.methode != 'dmi' :
                self.fbb.SetValue(os.path.join(self.dbb.GetValue(), 'corpus.txt'))
            else :
                self.fbb.SetValue(os.path.join(os.path.dirname(self.dbb.GetValue()), 'corpus.txt'))
        #self.log.write('FileBrowseButton: %s\n' % evt.GetString())

    def checkfile(self, evt) :
        if evt.GetId() == wx.ID_OK :
            if self.dbb.GetValue() != "" :
                if self.methode == 'dmi' :
                    if not os.path.exists(self.dbb.GetValue()) :
                        dlg = wx.MessageDialog(self, 
                        ' : '.join([self.dbb.GetValue(), _("this file doesn't exist")]), 'ATTENTION', wx.NO | wx.YES | wx.ICON_WARNING)
                        dlg.CenterOnParent()
                        if dlg.ShowModal() not in [wx.ID_NO, wx.ID_CANCEL]:
                            self.EndModal(wx.ID_OK)
                if os.path.exists(self.fbb.GetValue()):
                    dlg = wx.MessageDialog(self, 
                    "%s\nCe fichier existe, continuer quand même ?" % self.fbb.GetValue(), 'ATTENTION', wx.NO | wx.YES | wx.ICON_WARNING)
                    dlg.CenterOnParent()
                    if dlg.ShowModal() not in [wx.ID_NO, wx.ID_CANCEL]:
                        self.EndModal(wx.ID_OK)
                else :
                    self.EndModal(wx.ID_OK)
            else :
                dlg = wx.MessageDialog(self, "Vous devez choisir le répertoire contenant le ou les fichier(s) xml", 'ATTENTION', wx.OK | wx.ICON_WARNING)
                dlg.CenterOnParent()
                dlg.ShowModal()
        else :
            self.EndModal(wx.ID_CANCEL)


class ImportFactiva():

    def __init__(self, parent, methode):
        self.dial =  PrefImport(parent, methode=methode)
        self.dial.CenterOnParent()
        val = self.dial.ShowModal()
        if val == wx.ID_OK :
            xmldir = self.dial.dbb.GetValue()
            corp_out = self.dial.fbb.GetValue()
            self.dial.Destroy()
            busy = wx.BusyInfo(_("Please wait..."))
            wx.SafeYield()
            try :
                if methode == 'xml' :
                    res = getcorpus_from_xml(xmldir, corp_out)
                elif methode == 'mail' :
                    res = ParseFactivaMail(xmldir, corp_out, 'utf8', parent.syscoding)
                elif methode == 'txt' :
                    res = ParseFactivaPaste(xmldir, corp_out, 'utf8', parent.syscoding)
                elif methode == 'txm' :
                    res = TXM2IRA(xmldir, corp_out, 'utf8', parent.syscoding)
                elif methode == 'euro' :
                    res = ParseEuropress(xmldir, corp_out, 'utf8', 'utf8')
                del busy
                if res == 'nofile' :
                    dlg = wx.MessageDialog(parent, "Pas de fichiers dans %s" % xmldir, 'ATTENTION', wx.OK | wx.ICON_WARNING)
                    dlg.CenterOnParent()
                    dlg.ShowModal()
                    dlg.Destroy()
                else :
                    msg = '\n'.join([_("Corpus created :"), corp_out, _("Do you want to open it in IRaMuTeQ ?")])
                    dlg = wx.MessageDialog(parent, msg, _('Information'), wx.YES_NO | wx.ICON_INFORMATION | wx.STAY_ON_TOP)
                    dlg.CenterOnParent()
                    val = dlg.ShowModal()
                    if val == wx.ID_YES :
                        dlg.Destroy()
                        parent.filename = os.path.abspath(corp_out)
                        parent.OpenText()
                    else :
                        dlg.Destroy()
            except :
                del busy
                BugReport(parent)
        else :
            self.dial.Destroy()
