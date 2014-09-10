package bot;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.*;
import java.util.ArrayList;
import java.io.*;


public class MonthlyBot {
	public static final String[] URLS_OF_TABLES = {
		"http://www.eia.gov/petroleum/supply/monthly/csv/table1.csv",
		"http://www.eia.gov/petroleum/supply/monthly/csv/table5.csv",
		"http://www.eia.gov/petroleum/supply/monthly/csv/table9.csv",
		"http://www.eia.gov/petroleum/supply/monthly/csv/table13.csv",
		"http://www.eia.gov/petroleum/supply/monthly/csv/table17.csv",
	"http://www.eia.gov/petroleum/supply/monthly/csv/table21.csv"};


	public static int offset = 0;
	public static String currentDate="";

    public static void main(String[] args) throws Exception {
        ArrayList<int[]> rows;
        ArrayList<int[]> cols;
        String output = "";

        FileOutputStream out; // declare a file output object
        PrintStream p; // declare a print stream object

        try
        {
            // Create a new file output stream
            // connected to "myfile.txt"
            out = new FileOutputStream("0 Monthly Bot Output.csv");

            // Connect print stream to the output stream
            p = new PrintStream(out);
            // gets all the separate rows of data that need to be obtained
            cols = inputColumns();
            rows = inputRows();
            // outputs the headers to the file in csv format
            p.println(dataCodes());
            p.println(dataDescription());

            for(int i = 0; i < URLS_OF_TABLES.length; i++) {
                String url = URLS_OF_TABLES[i];
                output += runBot(url, rows.get(i), cols.get(i));
            }
            output = currentDate + output;
            p.println (output);

            p.close();
        }
        catch (Exception e)
        {
            System.err.println ("Error writing to file");
        }

	}

	/**
	 * Runs the master program using all other methods
	 * @param url
	 * @param rows
	 * @return A concatenated string of all data in CSV format
	 * @throws Exception
	 */
	public static String runBot (String url, int[] rows, int[] cols) throws Exception {
		// Variables
		ArrayList<String> s = new ArrayList<String>();
		String inputLine = "";
		String output= "";

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

		output+=findData(s, rows, cols);

		return output;
	}
	/**
	 * finds data within the CSV using a int toggle to check whether or not you are at the brgining or end of the string
	 * @param arrList
	 * @param rows
	 * @param cols
	 * @return
	 */
	public static String findData(ArrayList<String> arrList, int[] rows, int[] cols){ 
		String output = "";
		for(int i = 0; i < rows.length; i++) {
			String row = arrList.get(rows[i]);
			int counter = 0;
			String builder="";
			int index = 0;
			boolean found = false;
			int start = 0;	
			
			while(counter < row.length() && !found) {
				// builds string in between commas
				// 0 means start
				// 1 means end
				
				if(row.charAt(counter) == '"' && start == 0){
						start = 1;
						builder+='"';
				}
				else if(row.charAt(counter) == '"' && start == 1){
					builder+='"';
					if(index == cols[i]){
						output += "," + builder;
						found = true;
					}
					start =0;
					index++;
				}
				else if(start == 1){
					builder += row.charAt(counter);
				}
				else
					builder = "";
				
				counter++;
			}
		}

		return output;
	}

	/**
	 * Every specific column that holds wanted data in order of the URL it exists in
	 * @return An int[] of column values
	 */
	public static ArrayList<int[]> inputColumns() {
		ArrayList<int[]> arr = new ArrayList<int[]>();

		//Totals
		int[] table1 = {2,4,5,6,7,9,10,9};
		arr.add(table1);

		// PADD1
		int[] table5 = {2,4,5,6,7,8,10,11,10};
		arr.add(table5);

		//PADD2
		int[] table9 = {2,4,5,6,7,8,10,11,10};
		arr.add(table9);

		//PADD3
		int[] table13 = {2,4,5,6,7,8,10,11,10};
		arr.add(table13);

		//PADD4
		int[] table17 = {2,4,5,6,7,8,10,11,10};
		arr.add(table17);

		//PADD5
		int[] table21 = {2,4,5,6,7,8,10,11,10};
		arr.add(table21);

		return arr;
	}

	/**
	 * Every specific row that holds wanted data in order of the URL it exists in
	 * @return An int[] of row values
	 */
	public static ArrayList<int[]> inputRows() {
		ArrayList<int[]> arr1 = new ArrayList<int[]>();

		//Totals
		int[] table1 = {14,14,14,14,14,14,14,23};
		arr1.add(table1);

		// PADD1
		int[] table5 = {14,14,14,14,14,14,14,14,23};
		arr1.add(table5);

		//PADD2
		int[] table9 = {14,14,14,14,14,14,14,14,23};
		arr1.add(table9);

		//PADD3
		int[] table13 = {14,14,14,14,14,14,14,14,23};
		arr1.add(table13);

		//PADD4
		int[] table17 = {14,14,14,14,14,14,14,14,23};
		arr1.add(table17);

		//PADD5
		int[] table21 = {14,14,14,14,14,14,14,14,23};
		arr1.add(table21);

		return arr1;
	}

	/**
	 * A CVS formatted string of the codes that appear above the columns on the output spreadsheet
	 */
	public static String dataCodes() {
		return ",M_EPOOXE_YNP_NUS_MBBL,MFEIMUS1,M_EPOOXE_VUA_NUS_MBBL," +
		"M_EPOOXE_SCG_NUS_MBBL,MFERIUS1,M_EPOOXE_VPP_NUS_MBBL,MFESTUS1," +
		"MGFUPUS1,M_EPOOXE_YNP_R10_MBBL,MFEIM_R10-Z00_1,M_EPOOXE_VNR_R10-Z0P_MBBL," +
		"M_EPOOXE_VUA_R10_MBBL,M_EPOOXE_SCG_R10_MBBL,MFERIP11,M_EPOOXE_VPP_R10_MBBL," +
		"MFESTP11,MGFUPP11,M_EPOOXE_YNP_R20_MBBL,MFEIMP21,M_EPOOXE_VNR_R20-Z0P_MBBL," +
		"M_EPOOXE_VUA_R20_MBBL,M_EPOOXE_SCG_R20_MBBL,MFERIP21,M_EPOOXE_VPP_R20_MBBL," +
		"MFESTP21,MGFUPP21,M_EPOOXE_YNP_R30_MBBL,MFEIMP31,M_EPOOXE_VNR_R30-Z0P_MBBL," +
		"M_EPOOXE_VUA_R30_MBBL,M_EPOOXE_SCG_R30_MBBL,MFERIP31,M_EPOOXE_VPP_R30_MBBL," +
		"MFESTP31,MGFUPP31,M_EPOOXE_YNP_R40_MBBL,MFEIM_R40-Z00_1,M_EPOOXE_VNR_R40-Z0P_MBBL," +
		"M_EPOOXE_VUA_R40_MBBL,M_EPOOXE_SCG_R40_MBBL,MFERIP41,M_EPOOXE_VPP_R40_MBBL," +
		"MFESTP41,MGFUPP41,M_EPOOXE_YNP_R50_MBBL,MFEIMP51,M_EPOOXE_VNR_R50-Z0P_MBBL," +
		"M_EPOOXE_VUA_R50_MBBL,M_EPOOXE_SCG_R50_MBBL,MFERIP51,M_EPOOXE_VPP_R50_MBBL,MFESTP51,MGFUPP51";
	}

	/**
	 * An ArrayList of Arraylist's containing strings to each row of data
	 */
	public static String dataDescription() {
		String output = "";
		ArrayList<String> str = new ArrayList<String>();
		str.add("Date");
		str.add("U.S. Renewable Fuels Plant and Oxygenate Plant Net Production of Fuel Ethanol (Thousand Barrels)");
		str.add("U.S. Imports of Fuel Ethanol (Thousand Barrels)");
		str.add("U.S. Supply Adjustment of Fuel Ethanol (Thousand Barrels)");
		str.add("U.S. Fuel Ethanol Stock Change (Thousand Barrels)");
		str.add("U.S. Refinery and Blender Net Input of Fuel Ethanol (Thousand Barrels)");
		str.add("U.S. Product Supplied of Fuel Ethanol (Thousand Barrels)");
		str.add("U.S. Ending Stocks of Fuel Ethanol (Thousand Barrels)");
		str.add("U.S. Product Supplied of Finished Motor Gasoline (Thousand Barrels)");
		str.add("East Coast (PADD 1) Renewable Fuels Plant and Oxygenate Plant Net Production of Fuel Ethanol (Thousand Barrels)");
		str.add("East Coast (PADD 1) Imports of Fuel Ethanol (Thousand Barrels)");
		str.add("\"East Coast (PADD 1) Net Receipts by Pipeline, Tanker, and Barge from Other PADDs of Fuel Ethanol (Thousand Barrels)\"");
		str.add("East Coast (PADD 1) Supply Adjustment of Fuel Ethanol (Thousand Barrels)");
		str.add("East Coast (PADD 1) Fuel Ethanol Stock Change (Thousand Barrels)");
		str.add("East Coast (PADD 1) Refinery and Blender Net Input of Fuel Ethanol (Thousand Barrels)");
		str.add("East Coast (PADD 1) Product Supplied of Fuel Ethanol (Thousand Barrels)");
		str.add("East Coast (PADD 1) Ending Stocks of Fuel Ethanol (Thousand Barrels)");
		str.add("East Coast (PADD 1) Product Supplied of Finished Motor Gasoline (Thousand Barrels)");
		str.add("Midwest (PADD 2) Renewable Fuels Plant and Oxygenate Plant Net Production of Fuel Ethanol (Thousand Barrels)");
		str.add("Midwest (PADD 2) Imports of Fuel Ethanol (Thousand Barrels)");
		str.add("\"Midwest (PADD 2) Net Receipts by Pipeline, Tanker, and Barge from Other PADDs of Fuel Ethanol (Thousand Barrels)\"");
		str.add("Midwest (PADD 2) Supply Adjustment of Fuel Ethanol (Thousand Barrels)");
		str.add("Midwest (PADD 2) Fuel Ethanol Stock Change (Thousand Barrels)");
		str.add("Midwest (PADD 2) Refinery and Blender Net Input of Fuel Ethanol (Thousand Barrels)");
		str.add("Midwest (PADD 2) Product Supplied of Fuel Ethanol (Thousand Barrels)");
		str.add("Midwest (PADD 2) Ending Stocks of Fuel Ethanol (Thousand Barrels)");
		str.add("Midwest (PADD 2) Product Supplied of Finished Motor Gasoline (Thousand Barrels)");
		str.add("Gulf Coast (PADD 3) Renewable Fuels Plant and Oxygenate Plant Net Production of Fuel Ethanol (Thousand Barrels)");
		str.add("Gulf Coast (PADD 3) Imports of Fuel Ethanol (Thousand Barrels)");
		str.add("\"Gulf Coast (PADD 3) Net Receipts by Pipeline, Tanker, and Barge from Other PADDs of Fuel Ethanol (Thousand Barrels)\"");
		str.add("Gulf Coast (PADD 3) Supply Adjustment of Fuel Ethanol (Thousand Barrels)");
		str.add("Gulf Coast (PADD 3) Fuel Ethanol Stock Change (Thousand Barrels)");
		str.add("Gulf Coast (PADD 3) Refinery and Blender Net Input of Fuel Ethanol (Thousand Barrels)");
		str.add("Gulf Coast (PADD 3) Product Supplied of Fuel Ethanol (Thousand Barrels)");
		str.add("Gulf Coast (PADD 3) Ending Stocks of Fuel Ethanol (Thousand Barrels)");
		str.add("Gulf Coast (PADD 3) Product Supplied of Finished Motor Gasoline (Thousand Barrels)");
		str.add("Rocky Mountain (PADD 4) Renewable Fuels Plant and Oxygenate Plant Net Production of Fuel Ethanol (Thousand Barrels)");
		str.add("Rocky Mountain (PADD 4) Imports of Fuel Ethanol (Thousand Barrels)");
		str.add("\"Rocky Mountain (PADD 4) Net Receipts by Pipeline, Tanker, and Barge from Other PADDs of Fuel Ethanol (Thousand Barrels)\"");
		str.add("Rocky Mountain (PADD 4) Supply Adjustment of Fuel Ethanol (Thousand Barrels)");
		str.add("Rocky Mountain (PADD 4) Fuel Ethanol Stock Change (Thousand Barrels)");
		str.add("Rocky Mountain (PADD 4) Refinery and Blender Net Input of Fuel Ethanol (Thousand Barrels)");
		str.add("Rocky Mountain (PADD 4) Product Supplied of Fuel Ethanol (Thousand Barrels)");
		str.add("Rocky Mountain (PADD 4) Ending Stocks of Fuel Ethanol (Thousand Barrels)");
		str.add("Rocky Mountain (PADD 4) Product Supplied of Finished Motor Gasoline (Thousand Barrels)");
		str.add("West Coast (PADD 5) Renewable Fuels Plant and Oxygenate Plant Net Production of Fuel Ethanol (Thousand Barrels)");
		str.add("West Coast (PADD 5) Imports of Fuel Ethanol (Thousand Barrels)");
		str.add("\"West Coast (PADD 5) Net Receipts by Pipeline, Tanker, and Barge from Other PADDs of Fuel Ethanol (Thousand Barrels)\"");
		str.add("West Coast (PADD 5) Supply Adjustment of Fuel Ethanol (Thousand Barrels)");
		str.add("West Coast (PADD 5) Fuel Ethanol Stock Change (Thousand Barrels)");
		str.add("West Coast (PADD 5) Refinery and Blender Net Input of Fuel Ethanol (Thousand Barrels)");
		str.add("West Coast (PADD 5) Product Supplied of Fuel Ethanol (Thousand Barrels)");
		str.add("West Coast (PADD 5) Ending Stocks of Fuel Ethanol (Thousand Barrels)");
		str.add("West Coast (PADD 5) Product Supplied of Finished Motor Gasoline (Thousand Barrels)");

		for(int i =0; i < str.size(); i++)
			output += str.get(i) + ",";

		return output;
	}
}

