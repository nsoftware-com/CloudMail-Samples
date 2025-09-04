/*
 * Cloud Mail 2024 Java Edition - Sample Project
 *
 * This sample project demonstrates the usage of Cloud Mail in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/cloudmail
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */

import java.io.*;
import cloudmail.*;

import static java.lang.Integer.parseInt;

public class office365 extends ConsoleDemo {

  static String command; // user's command
  static String[] argument; // arguments following command

  static String currFolder = ""; // current folder id

  public static void main(String[] args) {

    try {
      Office365 office = new Office365();

      // Instructions
      System.out.println(
        "This console application connects to an Office365 mailbox."
      + "OAuth is required for authentication. You will need to create or have access to an OAuth application.\n"
      + "Use Microsoft's Identity program to create your own OAuth applications.\n"
      + "Read more here: https://learn.microsoft.com/en-us/entra/identity-platform/v2-overview\n"
      );

      System.out.println("Please enter the ClientId and ClientSecret for your OAuth application.");
      office.getOAuth().setClientId(prompt("ClientId"));
      office.getOAuth().setClientSecret(prompt("ClientSecret"));
      office.getOAuth().setServerAuthURL("https://login.microsoftonline.com/common/oauth2/v2.0/authorize");
      office.getOAuth().setServerTokenURL("https://login.microsoftonline.com/common/oauth2/v2.0/token");
      office.getOAuth().setAuthorizationScope("user.read mail.readwrite mail.send mailboxsettings.readwrite");

      prompt("Press Enter to open a browser to complete the next step of OAuth authentication", "");
      office.authorize();

      System.out.println("Authorization Successful.");

      while (true) {
        DisplayMenu();
        System.out.print("> ");
        command = input();
        argument = command.split("\\s");
        if (argument.length == 0)
          continue;
        switch (argument[0].charAt(0)) {
          case 'l':
            office.listFolders(currFolder); // Lists the root child folders.

            for (int i = 0; i < office.getFolders().size(); i++)
            {
              System.out.println("  " + office.getFolders().item(i).getDisplayName());
            }
            break;

          case 's':
            // Get the folder ID
            boolean found = false;
            office.listFolders(currFolder); // Lists the root child folders.

            for (int i = 0; i < office.getFolders().size(); i++)
            {
              if (office.getFolders().item(i).getDisplayName().equals(argument[1]))
              {
                currFolder = office.getFolders().item(i).getId();
                found = true;
                break;
              }
            }
            if (!found) {
              System.out.println("No matching subfolder. Setting the current folder to the root folder.");
              currFolder = "";
            }
            break;

          case 'm':
            office.config("MessagePageSize = 15");
            office.listMessages(currFolder, "");

            int i=0;
            while (true) {
              if (i < office.getMessageInfo().size()){
                System.out.println("  " + i + ": " + office.getMessageInfo().item(i).getSubject());
              }
              else{
                if ("y".equals(prompt("List Additional Messages (y/n)", ":", "y")))
                  office.listMessages(currFolder, "");
                else
                  break;
              }
              i = i+1;
            }
            break;

          case 'v':
            int messageNum = parseInt(argument[1]);
            System.out.println("Subject: " + office.getMessageInfo().item(messageNum).getSubject());
            System.out.println("From: " + office.getMessageInfo().item(messageNum).getFrom());
            System.out.println("Date: " + office.getMessageInfo().item(messageNum).getReceivedDate());
            System.out.println("Content: " + office.getMessageInfo().item(messageNum).getBodyContent());
            break;

          case 'c':
            office.setMessageSubject(prompt("Subject", ":", "Test"));
            office.setMessageBodyContentType("Text");
            office.setMessageBodyContent(prompt("Body", ":", "This message was sent using the CloudMail Office365 Class."));
            office.setMessageTo(prompt("To"));

            office.sendMail(true);
            break;

          case '?':
            DisplayMenu();
            break;

          case 'q':
            return;
          default:
            System.out.println("Bad command / Not implemented in demo.");

        }
      }
    } catch (Exception ex) {
      System.out.println(ex.getMessage());
    }
  }

  private static void DisplayMenu() {
    System.out.println("Commands");
    System.out.println("l                   list folders");
    System.out.println("s <folder>          select a subfolder");
    System.out.println("m                   list messages");
    System.out.println("v <message number>  view the content of a listed message");
    System.out.println("c                   compose and send a message");
    System.out.println("?                   display options");
    System.out.println("q                   quit");
  }
}

class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
        return defaultVal;
      else
        return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof CloudMailException) {
      System.out.print(" (" + ((CloudMailException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



