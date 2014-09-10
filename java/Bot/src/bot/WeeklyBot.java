package bot;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.*;
import java.util.ArrayList;
import java.io.*;


public class WeeklyBot {
	public static final String[] URLS_OF_TABLES = {"http://ir.eia.gov/wpsr/table1.csv",
		"http://ir.eia.gov/wpsr/table1.csv",
		"http://ir.eia.gov/wpsr/table1.csv",
		"http://ir.eia.gov/wpsr/table2.csv",
		"http://ir.eia.gov/wpsr/table9.csv",
		"http://ir.eia.gov/wpsr/table3.csv",
		"http://ir.eia.gov/wpsr/table7.csv",
		"http://ir.eia.gov/wpsr/table9.csv",
		"http://ir.eia.gov/wpsr/table2.csv",
		"http://ir.eia.gov/wpsr/table9.csv"};
	public static int offset = 0;
	public static String currentDate="";

	public static void main(String[] args) throws Exception {
        ArrayList<int[]> rows;
        String output = "";

        FileOutputStream out; // declare a file output object
        PrintStream p; // declare a print stream object


        try
        {
            // Create a new file output stream
            // connected to "myfile.txt"
            out = new FileOutputStream("0 Weekly Bot Output.csv");

            // Connect print stream to the output stream
            p = new PrintStream(out);
            // gets all the separate rows of data that need to be obtained
            rows = inputColumns();
            // outputs the headers to the file in csv format
            p.println(dataCodes());
            p.println(dataDescription());

            for(int i = 0; i < URLS_OF_TABLES.length; i++) {
                String url = URLS_OF_TABLES[i];
                output += runBot(url, rows.get(i), i);
            }
            output = currentDate + output;
            p.println (output);

            p.close();
        }
        catch (Exception e)
        {
            System.err.println ("Error writing to file: " + e.toString());
        }

	}

	/**
	 * Runs the master program using all other methods
	 * @param url
	 * @param rows
	 * @param offset
	 * @return A concatenated string of all data in CSV format
	 * @throws Exception
	 */
	public static String runBot (String url, int[] rows, int offset) throws Exception {
        // Variables
        ArrayList<String> s = new ArrayList<String>();
        String inputLine = "";
        String output= "";
        String dateLine = "";

        // URL steam and Reading buffer
        URL table = new URL(url);
        HttpURLConnection conn = (HttpURLConnection) table.openConnection();
        conn.connect();
        BufferedReader in = new BufferedReader(new InputStreamReader(conn.getInputStream()));

        // reads lines of the table into a ArrayList
        while ((inputLine = in.readLine()) != null)
            s.add(inputLine);

        //close stream
        in.close();
        dateLine = s.get(0);

        currentDate = compareDates(dateLine);
        String date = compareDates(dateLine);
        if(offset == 0 || offset == 2)
            output+=findData(s, findColumn(dateLine, date)+1, rows);
        else
            output+=findData(s, findColumn(dateLine, date), rows);

        return output;
	}

	/**
	 * Finds the actual data
	 * @param arrList
	 * @param colDates
	 * @param rows
	 * @return A string of the data specified
	 */
	public static String findData(ArrayList<String> arrList, int colDates, int[] rows){
		String output = "";
		for(int i = 0; i < rows.length; i++) {
			String row = arrList.get(rows[i]);
			int counter = 0;
			String builder="";
			int index = 0;
			boolean found = false;

			while(counter < row.length() && !found) {
				// builds string inbetween commas
				if(row.charAt(counter) == ',' && counter < row.length()-1 && row.charAt(counter+1) == '"'){
					if(index == colDates){
						//System.out.println(builder);
						output+= "," + builder;
						found = true;
					}
					else {
						index++;
						builder="";
					}
				}

				else
					builder+=row.charAt(counter);

				counter++;
			}
		}

		return output;
	}

	/**
	 * Compares all dates within the data then finds the most recent
	 * @param str
	 * @return A string containing the most recent date
	 */
	public static String compareDates(String str) {
		int counter = 0;
		String builder = "";
		ArrayList<String> dates = new ArrayList<String>();
		// builds an ArrayList of all dates
		while(counter < str.length()){
			if(str.charAt(counter)!=(',')){
				builder += str.charAt(counter);
			}
			else{
				int subcounter =0;
				int slashes = 0;
				while(subcounter < builder.length()){
					if(builder.charAt(subcounter) == '/')
						slashes++;
					subcounter++;
				}

				if(slashes == 2)
					dates.add(builder.substring(1, builder.length()-1));

				// resets builder string
				builder = "";
			}
			counter++;
		}

		counter = 0;
		String mostRecent = dates.get(0);
		int whichCol = 0;
		while(counter < dates.size()){
			String testDate = dates.get(counter);
			int recent = Integer.parseInt(mostRecent.substring(mostRecent.length()-2), 10);
			int temp = Integer.parseInt(testDate.substring(testDate.length()-2), 10);
			// checks year
			if(temp > recent){
				mostRecent = testDate;
				whichCol = counter;
			}
			// if equal checks month
			if(temp == recent){
				// finds offset on month
				int offSetRecent = offSet(mostRecent);
				int offSetTest = offSet(testDate);

				recent = Integer.parseInt(mostRecent.substring(0,offSetRecent), 10);
				temp = Integer.parseInt(testDate.substring(0,offSetTest), 10);
				// checks month
				if(temp > recent){
					mostRecent = testDate;
					whichCol = counter;
				}
				// if equal checks day
				if(temp == recent){
					recent = Integer.parseInt(mostRecent.substring(offSetRecent+1, mostRecent.length()-3), 10);
					temp = Integer.parseInt(testDate.substring(offSetTest+1, testDate.length()-3), 10);

					if(temp > recent){
						mostRecent = testDate;
						whichCol = counter;
					}
				}
			}
			counter++;
		}
		return "\"" + mostRecent + "\"";
	}
	/**
	 * Offsets date
	 * @param s
	 * @return an int
	 */
	public static int offSet(String s) {
		if(s.charAt(1) == '/')
			return 1;
		else
			return 2;
	}

	/**
	 * Finds the column that contains specified data
	 * @param row
	 * @param date
	 * @return An int that tells the program how many commas' over to look
	 */
	public static int findColumn(String row, String date) {
		int col =0;
		int counter = 0;
		String builder ="";

		while(counter < row.length()) {
			// builds string in between commas
			if(row.charAt(counter) != ',')
				builder+=row.charAt(counter);

			else{
				// checks if built string is the date we are looking for
				if(builder.equals(date))
					return col;
				col++;
				builder="";
			}
			counter++;
		}
		return -1;
	}

	/**
	 * Every specific column that holds wanted data in order of the URL it exists in
	 * @return An int[] of column values
	 */
	public static ArrayList<int[]> inputColumns() {
		ArrayList<int[]> arr = new ArrayList<int[]>();
		int[] table1Data2 = {34, 37, 38, 21, 22};
		arr.add(table1Data2);

		int[] table1Data1 = {2, 4, 8, 10};
		arr.add(table1Data1);

		int[] table1Data22 = {47, 49};
		arr.add(table1Data22);

		int[] table2Data1 = {19};
		arr.add(table2Data1);

		int[] table9Data3 = {64, 72, 78, 84, 90, 96, 102, 108, 114, 138};
		arr.add(table9Data3);

		int[] table3Data1 = {28, 29, 30, 31, 32, 33, 34, 35};
		arr.add(table3Data1);

		int[] table7Data1 = {21,25};
		arr.add(table7Data1);

		int[] table9Data6 = {274, 275, 276, 277, 278, 279, 58, 59, 60, 61, 62, 63, 78, 79, 80, 81, 82, 83, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107};
		arr.add(table9Data6);

		int[] table2Data11 = {61,62,63,64,65,66};
		arr.add(table2Data11);

		int[] table9Data2 = {447,448,449,450,451};
		arr.add(table9Data2);

		return arr;
	}

	/**
	 * A CVS formatted string of the codes that appear above the columns on the output spreadsheet
	 */
	public static String dataCodes() {
		return ",WCRRIUS2,W_EPOOXR_YPT_NUS_MBBLD,W_EPOOXE_YOP_NUS_MBBLD," +
				"WCRFPUS2,W_EPC0_FPF_SAK_MBBLD,WCESTUS1,WGTSTUS1,W_EPOOXE_SAE_NUS_MBBL," +
				"WDISTUS1,WGFUPUS2,WGFUPUS2,WPULEUS3,WGFRPUS2,WGRRPUS2,WG1TP_NUS_2," +
				"W_EPM0RO_YPT_NUS_MBBLD,WG4TP_NUS_2,WG5TP_NUS_2,W_EPM0CAL55_YPT_NUS_MBBLD," +
				"W_EPM0CAG55_YPT_NUS_MBBLD,WG6TP_NUS_2,WDIRPUS2,W_EPM0R_YPB_NUS_MBBLD," +
				"W_EPM0RA_YPB_NUS_MBBLD,W_EPM0RO_YPB_NUS_MBBLD,W_EPM0C_YPB_NUS_MBBLD," +
				"W_EPM0CA_YPB_NUS_MBBLD,W_EPM0CAL55_YPB_NUS_MBBLD,W_EPM0CAG55_YPB_NUS_MBBLD," +
				"W_EPM0CO_YPB_NUS_MBBLD,WGTIMUS2,W_EPOOXE_IM0_NUS-Z00_MBBLD,W_EPOOXE_SAE_NUS_MBBL,W_EPOOXE_SAE_R10_MBBL," +
				"W_EPOOXE_SAE_R20_MBBL,W_EPOOXE_SAE_R30_MBBL,W_EPOOXE_SAE_R40_MBBL,W_EPOOXE_SAE_R50_MBBL"
				+",W_EPOOXE_YIR_NUS_MBBLD,W_EPOOXE_YIR_R10_MBBLD,W_EPOOXE_YIR_R20_MBBLD,W_EPOOXE_YIR_R30_MBBLD," +
				"W_EPOOXE_YIR_R40_MBBLD,W_EPOOXE_YIR_R50_MBBLD,WG1TP_NUS_2,WG1TP_R10_2,WG1TP_R20_2,WG1TP_R30_2," +
				"WG1TP_R40_2,WG1TP_R50_2,WG5TP_NUS_2,WG5TP_R10_2,WG5TP_R20_2,WG5TP_R30_2,WG5TP_R40_2,WG5TP_R50_2," +
				"W_EPM0CAL55_YPT_NUS_MBBLD,W_EPM0CAL55_YPT_R10_MBBLD,W_EPM0CAL55_YPT_R20_MBBLD,W_EPM0CAL55_YPT_R30_MBBLD," +
				"W_EPM0CAL55_YPT_R40_MBBLD,W_EPM0CAL55_YPT_R50_MBBLD,W_EPOOXE_YOP_NUS_MBBLD,W_EPOOXE_YOP_R10_MBBLD,W_EPOOXE_YOP_R20_MBBLD," +
				"W_EPOOXE_YOP_R30_MBBLD,W_EPOOXE_YOP_R40_MBBLD,W_EPOOXE_YOP_R50_MBBLD";
	}

	/**
	 * A CVS formatted string of all the column descriptions that go above the data on the spreadsheet
	 */
	public static String dataDescription() {
		String output = "";
		ArrayList<String> str = new ArrayList<String>();
		str.add("Date");
		str.add("Weekly U.S. Crude Oil Inputs into Refineries  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Production of Oxygenates and Renewable Fuels  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Crude Oil Field Production  (Thousand Barrels per Day)");
		str.add("Weekly Alaska Refinery Field Production of Crude Oil  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Crude Oil Ending Stocks Excluding SPR  (Thousand Barrels)");
		str.add("Weekly U.S. Total Gasoline Ending Stocks  (Thousand Barrels)");
		str.add("Weekly U.S. Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)");
		str.add("Weekly U.S. Total Distillate Ending Stocks  (Thousand Barrels)");
		str.add("Weekly U.S. Finished Motor Gasoline Product Supplied  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Total Distillate Fuel Oil Product Supplied  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Percent Utilization of Refinery Operable Capacity  (Percent)");
		str.add("Weekly U.S. Refinery and Blender Adjusted Net Production of Finished Motor Gasoline (Thousand Barrels per Day)"); //!!!!
		str.add("Weekly U.S. Refinery and Blender Net Production of Finished Reformulated Motor Gasoline   (Thousand Barrels per Day)");
		str.add("Weekly U.S. Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol   (Thousand Barrels per Day)");
		str.add("Weekly U.S. Refinery and Blender Net Production of Other Finished Reformulated Motor Gasoline  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Refinery and Blender Net Production of Finished Conventional Motor Gasoline  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol  (Thousand Barrels per Day)");
		str.add("\"Weekly U.S. Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)\"");
		str.add("\"Weekly U.S. Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Greater than Ed 55  (Thousand Barrels per Day)\"");
		str.add("Weekly U.S. Refinery and Blender Net Production Other Finished Conventional Motor Gasoline  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Refinery and Blender Net Production of Distillate Fuel Oil   (Thousand Barrels per Day)");
		str.add("Weekly U.S. Blender Production of Gasoline Reformulated  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Blender Production of Reformulated Gasoline with Alcohol  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Blender Net Production of Finished Motor Gasoline Reformulated Other  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Blender Production of Conventional Gasoline  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Blender Production of Conventional Gasoline with Alcohol  (Thousand Barrels per Day)");
		str.add("\"Weekly U.S. Blender Net Production of Motor Gasoline, Finished, Conventional, Ed 55 & <  (Thousand Barrels per Day)\"");
		str.add("\"Weekly U.S. Blender Net Production of Motor Gasoline, Finished, Conventional, > Ed 55  (Thousand Barrels per Day)\"");
		str.add("Weekly U.S. Blender Production of Conventional Gasoline Other  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Total Gasoline Imports  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Imports of Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekl U.S. Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)");
		str.add("Weekly East Coast (PADD 1) Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)");
		str.add("Weekly Midwest (PADD 2) Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)");
		str.add("Weekly Gulf Coast (PADD 3) Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)");
		str.add("Weekly Rocky Mountain (PADD 4) Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)");
		str.add("Weekly West Coast (PADD 5) Ending Stocks of Oxygenates Fuel Ethanol  (Thousand Barrels)");
		str.add("Weekly U.S. Refinery and Blender Net Input of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly East Coast (PADD 1) Refinery and Blender Net Input of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly Midwest (PADD 2) Refinery and Blender Net Input of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly Gulf Coast (PADD 3) Refinery and Blender Net Input of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly Rocky Mountain (PADD 4) Refinery and Blender Net Input of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly West Coast (PADD 5) Refinery and Blender Net Input of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly U.S. Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol   (Thousand Barrels per Day)");
		str.add("Weekly East Coast (PADD 1) Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol   (Thousand Barrels per Day)");
		str.add("Weekly Midwest (PADD 2) Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly Gulf Coast (PADD 3) Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol   (Thousand Barrels per Day)");
		str.add("Weekly Rocky Mountain (PADD 4) Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol   (Thousand Barrels per Day)");
		str.add("Weekly West Coast (PADD 5) Refinery and Blender Net Production of Finished Reformulated Motor Gasoline with Ethanol   (Thousand Barrels per Day)");
		str.add("Weekly U.S. Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly East Coast (PADD 1) Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly Midwest (PADD 2) Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol (Thousand Barrels per Day)");
		str.add("Weekly Gulf Coast (PADD 3) Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly Rocky Mountain (PADD 4) Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly West Coast (PADD 5) Refinery and Blender Net Production of Finished Conventional Motor Gasoline with Ethanol  (Thousand Barrels per Day)");
		str.add("\"Weekly U.S. Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)\"");
		str.add("\"Weekly East Coast (PADD 1) Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)\"");
		str.add("\"Weekly Midwest (PADD 2) Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)\"");
		str.add("\"Weekly Gulf Coast (PADD 3) Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)\"");
		str.add("\"Weekly Rocky Mountain (PADD 4) Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)\"");
		str.add("\"Weekly West Coast (PADD 5)  Refinery and Blender Net Production of Finished Conventional Motor Gasoline, Ed 55 and Lower  (Thousand Barrels per Day)\"");
		str.add("Weekly U.S. Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly East Coast (PADD 1) Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly Midwest (PADD 2) Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly Gulf Coast (PADD 3) Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly Rocky Mountain (PADD 4) Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly West Coast (PADD 5) Oxy Plant Production of Oxygenates Fuel Ethanol  (Thousand Barrels per Day)");
		str.add("Weekly Fuel Ethanol Imports East Coast (PADD 1)");
		str.add("Weekly Fuel Ethanol Imports Midwest (PADD 2)");
		str.add("Weekly Fuel Ethanol Imports Gulf Coast (PADD 3)");
		str.add("Weekly Fuel Ethanol Imports Rocky Mountain (PADD 4)");
		str.add("Weekly Fuel Ethanol Imports West Coast (PADD 5)");

		for(int i =0; i < str.size(); i++)
			output += str.get(i) + ",";

		return output.substring(0, output.length()-1);
	}
}
