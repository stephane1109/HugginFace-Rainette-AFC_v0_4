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
from operator import itemgetter

#------------------------------------
# import des modules wx
#------------------------------------
import wx

#------------------------------------
# import des fichiers du projet
#------------------------------------
from chemins import ffr, FFF
from analysematrix import AnalyseMatrix
from functions import exec_rcode, check_Rresult
from dialog import FreqDialog
from PrintRScript import PrintRScript, FreqMultiScript


class Frequences(AnalyseMatrix) :

    def doparametres(self, dlg=None) :
        if dlg is None :
            return
        else :
            dial = FreqDialog(self.parent, self.tableau.get_colnames(), "Fréquences")
            dial.CenterOnParent()
            val = dial.ShowModal()
            if val == wx.ID_OK :
                self.parametres['colsel'] = dial.m_listBox1.GetSelections()
                self.parametres['header'] = dial.header
                self.parametres['NA'] = dial.includeNA.GetValue()
            else :
                self.parametres = None
            dial.Destroy()

    def doanalyse(self):
        self.pathout.createdir(self.parametres['pathout'])
        header = self.tableau.get_colnames()
        select = self.parametres['colsel']
        self.listtitre = [header[i] for i in select]
        b, self.outframe = tempfile.mkstemp()
        self.fileforR = [ffr(os.path.join(self.pathout.dirout, 'freq_%i.png' % i)) for i in range(len(select))]
        self.Rscript = PrintRScript(self)
        sel = 'c(' + ','.join([str(val + 1) for val in select]) + ')'
        listfiles = 'c("' + '","'.join(self.fileforR) + '")'
        titles = 'c("' +  '","'.join(self.listtitre) + '")'
        txt = """
        filein <- "%s"
        encoding <- '%s'
        dm <- read.csv2(filein, encoding = encoding, header = TRUE, row.names = 1, sep='\\t', quote = '"', na.string = '')
        """ %(ffr(self.tableau.parametres['csvfile']), self.tableau.parametres['syscoding'])
        txt += """
        outframe <- data.frame(cbind('***','****','****'))
        colnames(outframe)<-c('effectif','pourcentage', 'labels')
        select <- %s
        listfiles <- %s
        titles <- %s
        compteur <- 1
        """ % (sel, listfiles, titles)
        if self.parametres['NA'] :
            txt += """
            countNA <- TRUE
            """
        else :
            txt += """
            countNA <- FALSE
            """
        txt += """
        for (i in select) {
            if (countNA) {
                freq <- table(dm[,i], useNA = 'ifany')
            } else {
                freq <- table(dm[,i])
            }
            sumfreq <- sum(freq)
            pour <- prop.table(as.matrix(freq), 2) * 100
            sumpour <- sum(pour)
            pour <- round(pour, 2)
            ntable <- cbind(as.matrix(freq), pour)
            graphout <- listfiles[compteur]  
            if (Sys.info()["sysname"]=='Darwin') {
                quartz(file=graphout,type='png')
                par(cex=1)
            } else {
                png(graphout)
                par(cex=0.3)
                }
            if (max(nchar(rownames(ntable))) > 15) {
                lab.bar <- 1:nrow(ntable)
            } else {
                lab.bar <- rownames(ntable)
            }
            barplot(ntable[,2],border=NA,beside=TRUE,names.arg=lab.bar)
            ntable <- cbind(ntable, rownames(as.matrix(freq)))
            colnames(ntable) <- c('effectif','pourcentage', 'labels')
            title(main=titles[compteur])
            dev.off()
            ntable<-rbind(ntable,total=c(sumfreq,sumpour,''))
            outframe<-rbind(outframe,c('***','****','****'))
            #datasum[,1]<-as.character(datasum[,1])
            #datasum[,2]<-as.character(datasum[,2])
            outframe<-rbind(outframe,ntable)
            compteur <- compteur + 1
        }
        outframe<-rbind(outframe,c('***','****','****'))
        write.table(outframe, file="%s", sep="\\t")
        """ % ffr(self.outframe)
        self.Rscript.add(txt)
        self.Rscript.write()
        self.doR(self.Rscript.scriptout)
        self.dolayout()

    def dolayout(self):
        listtab = []
        tab = []
        with open(self.outframe, 'r', encoding='utf8') as f :
            content = f.read().splitlines()
        content.pop(0)
        content.pop(0)
        content = ['\t'.join(line.split('\t')[1:]).replace('"','') for line in content]
        content = '\n'.join(content)
        content = content.split('***\t****\t****')
        content = [[line.split('\t') for line in tab.splitlines() if line.split('\t') != ['']] for tab in content]
        listtab = [tab for tab in content if tab != []]
        texte = ''
        #for ligne in content:
        #    ligne = ligne.replace('"', '')
        #    ligne = ligne.split('\t')
        #    if ligne[1] == '***' :
        #        if tab != []:
        #            listtab.append(tab)
        #        tab = []
        #    else :
        #        tab.append(ligne)
        pretexte = '''<html>
        <meta http-equiv="content-Type" content="text/html; charset=utf8" />
        <body>\n<h1>Fréquences</h1>
        <a name="deb"></a><br>
        ''' 
        for i in range(0, len(listtab)):
            pretexte += '<p><a href="#%s">%s</a></p>' % (str(i), self.listtitre[i])
            texte += '<hr size="5" align="center" width="50%" color="green">\n'
            texte += '<p><a href="#deb">Retour</a></p>\n'
            texte += '<a name="%s"></a><h2>%s</h2>\n' % (str(i), self.listtitre[i])
            texte += '<table>\n<tr><td>\n'
            texte += '<table border=1><tr><td></td><td>Effectifs</td><td>pourcentage</td></tr>'
            for line in listtab[i] :
                texte += '<tr>'
                texte += """
                <td>%s</td><td align=center>%s</td><td align=center>%s %%</td>
                """ % (line[2], line[0], line[1])
                texte += '</tr>'
            texte += '</table></td>'
            texte += """
            <td><img src="%s" alt="graph"/></td></tr></table>\n
            """ % os.path.basename(self.fileforR[i])
            texte += '</body>\n</html>'
        fileout = os.path.join(self.pathout.dirout, 'resultats.html')
        with open(fileout, 'w', encoding='utf8') as f :
            f.write(pretexte + texte)
        #return fileout


class FreqMultiple(Frequences): 

    def doanalyse(self):
        select = self.parametres['colsel']
        freq = self.tableau.countmultiple(select)
        tot = sum([freq[forme][0] for forme in freq])
        freq = [[forme, freq[forme][0], repr(round((float(freq[forme][0])/tot)*100, 2)),repr(len(list(set(freq[forme][1])))), repr(round((float(len(list(set(freq[forme][1]))))/self.tableau.rownb)*100,2))] for forme in freq]
        freq = sorted(freq, key=itemgetter(1), reverse=True)
        freq = [[line[0], repr(line[1]), line[2], line[3], line[4]] for line in freq]
        freq.insert(0, ['mod', 'freq', 'percent of total', 'row number', 'percent of rows'])
        self.freq = freq
        with open(self.pathout['frequences.csv'], 'w', encoding='utf8') as f :
            f.write('\n'.join(['\t'.join(line) for line in freq]))
        self.rscript = FreqMultiScript(self)
        self.rscript.make_script()
        self.doR(self.rscript.scriptout)
        self.dolayout()

    def dolayout(self):
        pretexte = '''<html>
        <meta http-equiv="content-Type" content="text/html; charset=utf8" />
        <body>\n<h1>Fréquences</h1>
        <a name="deb"></a><br>
        '''       
        txt = """
        <table>\n<tr><td>\n
        <table border=1><tr><td>
        """
        txt += '</td></tr><tr><td>'.join(['</td><td>'.join(line) for line in self.freq]) + '</td></tr></table></td></tr>'
        txt += '<tr><td><img src="%s" alt="graph"/></td><td><img src="%s" alt="graph"/></td></tr></table>' % (os.path.basename(self.pathout['barplotfreq.png']), os.path.basename(self.pathout['barplotrow.png']))
        txt += "</body>\n</html>"
        with open(self.pathout['resultats.html'], 'w', encoding='utf8') as f :
            f.write(pretexte + txt)
