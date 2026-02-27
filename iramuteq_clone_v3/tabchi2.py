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
import HTML
from chemins import ffr,FFF
from functions import exec_rcode, check_Rresult
from dialog import ChiDialog, PrefChi
from analysematrix import AnalyseMatrix


chioption = { 'valobs' : True,
              'valtheo' : True,
              'resi' : False,
              'contrib' : True,
              'pourcent' : False,
              'pourcentl' : True,
              'pourcentc' : True,
              'graph' : True,
              'bw' : False,
              }


def make_res(line) :
    if float(line[5]) <= 0.05 and line[6] != 'warning':
        line.append('green')
    elif float(line[5]) <= 0.05 and line[6] == 'warning':
        line.append('blue')
    else :
        line.append('red')
    return line

def clean_line(result) :
    return [[val for val in line if val != '**'] for line in result]

def make_table(tabs, tab_title, res) :
    return ['<br>'.join(['<font color=%s>%s</font>' % (res[i][-1], tab_title), HTML.table(tab)]) for i,tab in enumerate(tabs)]

def make_restab(res) :
    return ['<br>'.join(['<font color=%s>%s</font>'% (line[-1], 'Résultats'),HTML.table([['chi', line[3]],['p', line[5]]])]) for i,line in enumerate(res)]

def make_htmlgraphs(graphs) :
    return ['<img src=%s>' % os.path.basename(val) for val in graphs]

def make_link_list(res, text) :
    return ['<a name=back_%i></a><a href=#%i><font color=%s>%s</font></a>' % (i, i, chi[-1], text[i]) for i, chi in enumerate(res)]

def make_title(res, text) :
    return ['<a name=%i></a><br><font color=%s>%s</font><br><a href=#back_%i>retour</a><br>' % (i, val[-1], text[i], i) for i, val in enumerate(res)] 


class ChiSquare(AnalyseMatrix):

    def doparametres(self, dlg = None):
        if dlg is None :
            return
        dial = ChiDialog(self.parent, -1, "Chi2", chioption, self.tableau, size=(400, 350),
                     style = wx.DEFAULT_DIALOG_STYLE
                     )
        dial.CenterOnParent()
        val = dial.ShowModal()
        if val==wx.ID_OK :     
            self.colsel1 = dial.list_box_1.GetSelections()
            self.colsel2 = dial.list_box_2.GetSelections()
            if dial.chiopt :
                chioption['valobs'] = dial.dial.check1.GetValue()
                chioption['valtheo'] = dial.dial.check2.GetValue()
                chioption['resi'] = dial.dial.check3.GetValue()
                chioption['contrib'] = dial.dial.check4.GetValue()
                chioption['pourcent'] = dial.dial.check5.GetValue()
                chioption['pourcentl'] = dial.dial.check6.GetValue()
                chioption['pourcentc'] = dial.dial.check7.GetValue()
                chioption['graph'] = dial.dial.check8.GetValue()
                chioption['bw'] = dial.dial.checkbw.GetValue()
                dial.dial.Destroy()
            dial.Destroy()
            self.parametres.update(chioption)
            self.chioption = chioption
        else :
            if dial.chiopt :
                dial.dial.Destroy()
            dial.Destroy()
            self.parametres = None

    def doanalyse(self):
        self.count = 1
        keepGoing = self.dlg.Update(self.count,"Analyse dans R...")
        self.OutFrame=tempfile.mktemp(dir=self.parent.TEMPDIR)
        self.encode=self.parent.encode
        self.TEMPDIR=self.parent.TEMPDIR
        self.RPath=self.parent.PathPath.get('PATHS','rpath')
        self.TextCroise=[]
        for i in self.colsel1 :
            for j in self.colsel2 :
                self.TextCroise.append(self.tableau.colnames[i] + ' / ' + self.tableau.colnames[j])
        rchioption = {}
        for val in self.chioption :
            if self.chioption[val]:
                rchioption[val] = 'TRUE'
            else :
                rchioption[val] = 'FALSE'
        txt="""
        source("%s")
        """%ffr(self.parent.RscriptsPath['Rfunct'])
        txt += """
        source("%s")
        """ % ffr(self.parent.RscriptsPath['Rgraph'])
        txt += """
        doobs <- %s
        doexp <- %s
        docontrib <- %s
        doresi <- %s
        dopr <- %s
        doprl <- %s
        doprc <- %s
        dograph <- %s
        bw <- %s
        """ % (rchioption['valobs'], rchioption['valtheo'], rchioption['contrib'], rchioption['resi'], rchioption['pourcent'], rchioption['pourcentl'], rchioption['pourcentc'], rchioption['graph'], rchioption['bw'])
        txt+="""
        datadm <- read.csv2("%s", encoding="%s", header = TRUE, row.names = 1, sep='\\t', quote = '"', na.string = '')
        listres<-list()
        listcol<-list()
        cont<-1
        """%(ffr(self.tableau.parametres['csvfile']), self.tableau.parametres['syscoding'])
        if len(self.colsel1)==1:
            strsel1=str(tuple(self.colsel1)).replace(',','')
        else:
            strsel1=str(tuple(self.colsel1))
        if len(self.colsel2)==1:
            strsel2=str(tuple(self.colsel2)).replace(',','')
        else:
            strsel2=str(tuple(self.colsel2))
        txt+="""
        for (i in c%s) {""" % strsel1
        txt+="""
            for (j in c%s) {""" % strsel2
        txt+="""
                tab<-table(datadm[,i+1],datadm[,j+1])
                if (min(dim(tab)) != 1) {
                    chi<-chisq.test(tab)
                    CS<-colSums(tab)
                    RS<-rowSums(tab)
                    GT<-sum(tab)
                    chi$contrib<-(tab-chi$expected)/sqrt(chi$expected * ((1 - RS/GT) %%*%% t(1 - CS/GT)))
                    listres[[cont]]<-chi
                    listcol[[cont]]<-ncol(tab)
                    cont<-cont+1
                } else {
                    chi <- list(observed = tab, residuals = tab, contrib = tab, statistic = 0, p.value = 1, expected = tab, message = 'pas de calcul')
                    listres[[cont]] <- chi
                    listcol[[cont]]<-ncol(tab)
                    cont <- cont + 1
                }
            }
        }
        maxcol<-max(unlist(listcol))+1
        if (maxcol<7) {maxcol<-7}
        frameout<-matrix('*',1,maxcol)
        count<-0
        for (chi in listres) {
            if (min(chi$expected)<5) {
                att<-"warning"
            } else {
                att<-""
            }
            if ('message' %%in%% attributes(chi)$names) {
                att <- "Ce chi2 n\'a pas été calculé"
                nom_colresi<-colnames(chi$observed)
                chi$prl <- chi$expected
                chi$prc <- chi$expected
                st <- sum(chi$observed)
            } else {
                nom_colresi<-colnames(chi$observed)
                st <- sum(chi$observed)
                sc <- colSums(chi$observed)
                sr <- rowSums(chi$observed)
                chi$prl <- round((chi$observed/sr)*100,2)
                chi$prc <- t(round((t(chi$observed)/sc)*100,2))
            }
            fileout<-paste('histo_',count,sep='')
            fileout<-paste(fileout,'.png',sep='')
            count<-count+1
            fileout<-file.path("%s",fileout)
            if (max(nchar(colnames(chi$observed)))>15) {
                leg <- 1:length(colnames(chi$observed))
            } else {
                leg <- colnames(chi$observed)
            }
            if (dograph) {
                width<-ncol(chi$observed)*100
                if (width < 350) {width <- 350}
                open_file_graph(fileout,width = width, height = 300)
                par(mar=c(0,0,0,0))
                layout(matrix(c(1,2),1,2, byrow=TRUE),widths=c(3,1))
                par(mar=c(2,2,1,0))
                par(cex=0.8)
                if (!bw) colors <- rainbow(length(rownames(chi$observed)))
                else colors <- gray.colors(length(rownames(chi$observed)))
                barplot(chi$prl,names.arg = leg, beside=TRUE,border=NA, col=colors)
                par(mar=c(0,0,0,0))
                par(cex=0.8)
                plot(0, axes = FALSE, pch = '')
                legend(x = 'center' , rownames(chi$observed), fill = colors)
                dev.off()
            }
            chi$prl <- cbind(chi$prl, total = rowSums(chi$prl))
            chi$prc <- rbind(chi$prc, total = colSums(chi$prc))
            chi$observed<-rbind(chi$observed,total=colSums(chi$observed))
            chi$observed<-cbind(chi$observed,total=rowSums(chi$observed))
            chi$pr <- round((chi$observed/st)*100,2)
            chi$expected<-rbind(chi$expected,total=colSums(chi$expected))
            chi$expected<-cbind(chi$expected,total=rowSums(chi$expected))
            chi$expected<-round(chi$expected,digits=2)
            chi$residuals<-round(chi$residuals,digits=2)
            chi$contrib<-round(chi$contrib, digits=2)
            nom_col<-colnames(chi$observed)
           
            if (ncol(chi$observed)<maxcol) {
                for (i in 1:(maxcol-ncol(chi$observed))) {
                    chi$observed<-cbind(chi$observed,'**')
                    chi$pr<-cbind(chi$pr,'**')
                    chi$prl<-cbind(chi$prl,'**')
                    chi$prc<-cbind(chi$prc,'**')
                    chi$expected<-cbind(chi$expected,'**')
                    chi$residuals<-cbind(chi$residuals,'**')
                    chi$contrib<-cbind(chi$contrib,'**')
                    nom_col<-append(nom_col,'**')
                    nom_colresi<-append(nom_colresi,'**')
                }
                chi$residuals<-cbind(chi$residuals,'**')
                chi$contrib<-cbind(chi$contrib,'**')
                nom_colresi<-append(nom_colresi,'**')
                chi$prc<-cbind(chi$prc,'**')
            } else if (ncol(chi$observed)==maxcol) {
                chi$residuals<-cbind(chi$residuals,'**')
                chi$contrib<-cbind(chi$contrib,'**')
                nom_colresi<-append(nom_colresi,'**')
                chi$prc<-cbind(chi$prc,'**')
            }
            if (doobs) {
                li<-matrix('*obs*',1,maxcol)
                frameout<-rbind(frameout,li)
                frameout<-rbind(frameout,nom_col)
                frameout<-rbind(frameout,chi$observed)
            }
            if (doexp) {
                li<-matrix('*exp*',1,maxcol)
                frameout<-rbind(frameout,li)
                frameout<-rbind(frameout,nom_col)
                frameout<-rbind(frameout,chi$expected)
            }
            if (doresi) {
                li<-matrix('*resi*',1,maxcol)
                frameout<-rbind(frameout,li)
                frameout<-rbind(frameout,nom_colresi)
                frameout<-rbind(frameout,chi$residuals)
            }
            if (docontrib) {
                li<-matrix('*contrib*',1,maxcol)
                frameout<-rbind(frameout,li)
                frameout<-rbind(frameout,nom_colresi)
                frameout<-rbind(frameout,chi$contrib)
            }
            if (dopr) {
                li<-matrix('*pr*', 1, maxcol)
                frameout<-rbind(frameout,li)
                frameout<-rbind(frameout,nom_col)
                frameout<-rbind(frameout,chi$pr)
            }
            if (doprl) {
                li<-matrix('*prl*', 1, maxcol)
                frameout<-rbind(frameout,li)
                frameout<-rbind(frameout,nom_col)
                frameout<-rbind(frameout,chi$prl)
            }
            if (doprc) {
                li<-matrix('*prc*', 1, maxcol)
                frameout<-rbind(frameout,li)
                frameout<-rbind(frameout,nom_colresi)
                frameout<-rbind(frameout,chi$prc)
            }
            res<-c('****','chi',chi$statistic,'p',chi$p.value,att,fileout)
            frameout<-rbind(frameout,res)
        }
        li<-matrix('fin_analyse',1,maxcol)
        frameout<-rbind(frameout,li)
        write.csv2(frameout,file="%s")
        """ % (ffr(self.parametres['pathout']),ffr(self.OutFrame))
        tmpfile=tempfile.mktemp(dir=self.TEMPDIR)
        print(tmpfile)
        tmpscript=open(tmpfile,'w', encoding='utf8')
        tmpscript.write(txt)
        tmpscript.close()
        pid = exec_rcode(self.RPath, tmpfile, wait = False)
        while pid.poll() == None :
            sleep(0.2)
        check_Rresult(self.parent, pid)            
        self.count += 1
        keepGoing = self.dlg.Update(self.count,"Ecriture des résultats")
        listfileout = self.dolayout(self.chioption)
        #listfileout=dlg.ShowChi2(ColSel1,ColSel2)
        #parent.FreqNum += 1
        #parent.DictTab[u"Chi2_%s*"%parent.FreqNum]=listfileout
        #parent.newtab = wx.html.HtmlWindow(parent.nb, -1)
        #if "gtk2" in wx.PlatformInfo:
            #parent.newtab.SetStandardFonts()
            #parent.newtab.LoadPage(listfileout[len(listfileout)-1])
            #parent.nb.AddPage(parent.newtab,u"Chi2_%s*"%parent.FreqNum)
            #parent.nb.SetSelection(parent.nb.GetPageCount()-1)
            #parent.ShowTab(wx.EVT_BUTTON)
            #parent.DisEnSaveTabAs(True)
        #self.count += 1
        #keepGoing = self.dlg.Update(self.count,u"Fini")

    def dolayout(self, option):
        ListFile=[False]
        file=open(self.OutFrame,'r', encoding='utf8')
        content=file.readlines()
        file.close()
        lcont = [line.replace('"','').replace('\n','').split(';') for line in content]
        lcont.pop(0)
        lcont.pop(0)
        allcoord = []
        names = []
        res = [chi for chi in lcont if chi[0]=='res']
        res = [make_res(line) for line in res]
        coord_res = [i for i,chi in enumerate(lcont) if chi[0]=='res']
        if option['valobs']:
            allcoord.append([i for i,chi in enumerate(lcont) if chi[1]=='*obs*'])
            names.append('Valeurs observées')
        if option['valtheo'] :
            allcoord.append([i for i,chi in enumerate(lcont) if chi[1]=='*exp*'])
            names.append('Valeurs théoriques')
        if option['resi'] :
            allcoord.append([i for i,chi in enumerate(lcont) if chi[1]=='*resi*'])
            names.append('Residuals')
        if option['contrib'] :
            allcoord.append([i for i,chi in enumerate(lcont) if chi[1]=='*contrib*'])
            names.append('Contributions a posteriori')
        if option['pourcent'] : 
            allcoord.append([i for i,chi in enumerate(lcont) if chi[1]=='*pr*'])
            names.append('Pourcentages')
        if option['pourcentl'] :
            allcoord.append([i for i,chi in enumerate(lcont) if chi[1]=='*prl*'])
            names.append('Pourcentages en ligne')
        if option['pourcentc'] :
            allcoord.append([i for i,chi in enumerate(lcont) if chi[1]=='*prc*'])
            names.append('Pourcentages en colonne')
        allcoord.append(coord_res)
        allhtml = [[clean_line(lcont[allcoord[i][j]+1:allcoord[i+1][j]]) for j, line in enumerate(allcoord[i])] for i, tab in enumerate(allcoord) if i!=len(allcoord)-1]
        allhtml = [make_table(val,names[i],res) for i,val in enumerate(allhtml)]
        links = make_link_list(res, self.TextCroise)
        html_res = make_restab(res)
        allhtml.insert(0,html_res)
        titles = make_title(res, self.TextCroise)
        allhtml.insert(0,titles)
        if option['graph'] :
            graphs = [line[7] for line in res]
            ListFile += graphs
            html_graphs = make_htmlgraphs(graphs)
            allhtml.append(html_graphs)
        header="""
        <html>\n
        <meta http-equiv="content-Type" content="text/html; charset=utf8" />\n
        <body>\n
        <h1>Test du Chi2</h1>\n
        <br>
        <table border=1><tr><td>
        Légende : <br>
        <font color=green>p &lt;= 0.05</font><br>
        <font color=blue>p &lt;= 0.05 mais il y a des valeurs théoriques &lt; 5</font><br>
        <font color=red>p &gt; 0.05</font>
        </td></tr></table><br><br>
        """
        pretxt = '<br>\n'.join(links)+'<br><hr><br>\n'
        txt = '<br><hr><br>\n'.join(['<br><br>'.join([tab[i] for tab in allhtml]) for i,val in enumerate(res)])
        txt = header + pretxt + txt + '\n</body></html>'
        fileout=os.path.join(self.parametres['pathout'],'resultats-chi2.html')
        with open(fileout, 'w',encoding='utf8') as f :
            f.write(txt)
        ListFile.append(fileout)         
        return ListFile
