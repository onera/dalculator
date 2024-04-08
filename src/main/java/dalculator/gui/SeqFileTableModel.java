/*
 * Copyright (c)  2023. ONERA
 * This file is part of Dalculator
 *
 * Dalculator is free software ;
 * you can redistribute it and/or modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation ;
 * either version 2 of  the License, or (at your option) any later version.
 *
 * PML Analyzer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY ;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program ;
 *  if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package dalculator.gui;

import java.util.ArrayList;
import java.util.List;
import javax.swing.table.AbstractTableModel;

public class SeqFileTableModel extends AbstractTableModel {

	private final List<SeqFileDataModel> seqFileList= new ArrayList<>();
	private final String[] headers = {"Failure Condition", "nSev", "xSev", "refDAL"};

	public SeqFileTableModel() {
		super();	
	}

	@Override
	public boolean isCellEditable(int rowIndex, int columnIndex) {
	    return true; 
	}
	
	@Override
	public int getColumnCount() {
		return headers.length;
	}

	public String getColumnName(int columnIndex) {
        return headers[columnIndex];
    }
	
	@Override
	public Class getColumnClass(int columnIndex){
	    switch(columnIndex){
	        case 1:
	            return Integer.class;
	        case 2:
	            return Double.class;
	        case 3:
	            return String.class;
	        default:
	            return Object.class;
	    }
	}
	
	@Override
	public int getRowCount() {
		return seqFileList.size();
	}

	@Override
	public Object getValueAt(int rowIndex, int columnIndex) {
		switch(columnIndex){
		case 0:
			return seqFileList.get(rowIndex).getSeqFileName();
		case 1:
			return seqFileList.get(rowIndex).getNSev();
		case 2:
			return seqFileList.get(rowIndex).getXSev();
		case 3:
			return seqFileList.get(rowIndex).getRefDal();
		default:
			return null;

		}

	} //getValueAt
	
	@Override
	public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
	    if(aValue != null){
	        SeqFileDataModel seqFile = seqFileList.get(rowIndex);
	        switch(columnIndex){
	            case 0:
	                seqFile.setSeqFileName((String)aValue);
	                break;
	            case 1:
	                seqFile.setNSev((Integer)aValue);
	                break;
	            case 2:
	                seqFile.setXSev((Double)aValue);
	                break;
	            case 3:
	                seqFile.setRefDal((String)aValue);
	                break;
	        }
	    }
	} //setValueAt
	
	public void addSeqFile(SeqFileDataModel seqFile){
		seqFileList.add(seqFile);
		fireTableRowsInserted(seqFileList.size() -1, seqFileList.size() -1);
	}
	
	public void removeSeqFile(int rowIndex){
		seqFileList.remove(rowIndex);
		fireTableRowsDeleted(rowIndex, rowIndex);
	}
}
