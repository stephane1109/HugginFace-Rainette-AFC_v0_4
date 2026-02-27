# -*- coding: utf-8 -*-
#Author: Pierre Ratinaud
#Copyright (c) 2008-2020 Pierre Ratinaud
#modification pour python 3 : Laurent Mérat, 6x7 - mai 2020
#License: GNU/GPL

#------------------------------------
# import des modules wx
#------------------------------------
from wx.lib import sheet

# utilisé seulement dans layout.py


class MySheet(sheet.CSheet):

    def __init__(self, parent):
        sheet.CSheet.__init__(self, parent)
        self.parent=parent
        self.row = self.col = 0

    def OnGridSelectCell(self, event):
        self.row, self.col = event.GetRow(), event.GetCol()
        value =  self.GetColLabelValue(self.col) + self.GetRowLabelValue(self.row)
        event.Skip()
        
    def Populate(self,content):
        nrow=len(content)
        self.SetNumberRows(nrow)
        ncol=len(content[1])
        self.SetNumberCols(ncol)
        for y in range(0,nrow):
            for i in range(0,ncol):
                self.SetCellValue(y,i,str(content[y][i]))
