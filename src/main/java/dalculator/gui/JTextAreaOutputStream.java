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

import java.io.OutputStream;
import javax.swing.JTextArea;

public class JTextAreaOutputStream extends OutputStream {
	JTextArea ta;
	public JTextAreaOutputStream(JTextArea t) {
		super();
		ta = t;
	}
	public void write(int i) {
		ta.append(Character.toString((char)i));
	}
	public void write(char[] buf, int off, int len) {
		String s = new String(buf, off, len);
		ta.append(s);
	}
}

