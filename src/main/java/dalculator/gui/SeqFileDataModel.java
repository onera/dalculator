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

public class SeqFileDataModel {
	private String seqFileName;
	private Integer nSev;
        private String refDal;
	private Double xSev;
	
	public SeqFileDataModel(String filename, Integer nSev, Double xSev, String refDal) {
		this.seqFileName = filename;
		this.nSev = nSev;
		this.xSev = xSev;
                this.refDal = refDal;
	}

	public String getSeqFileName() {
		return seqFileName;
	}

	public void setSeqFileName(String seqFileName) {
		this.seqFileName = seqFileName;
	}

	public Integer getNSev() {
		return this.nSev;
	}

	public void setNSev(Integer nSev) {
		this.nSev = nSev;
	}

	public Double getXSev() {
		return this.xSev;
	}

	public void setXSev(Double xSev) {
		this.xSev = xSev;
	}
        public String getRefDal() {
		return this.refDal;
	}

        public void setRefDal(String refDal) {
		this.refDal = refDal;
	}
}
