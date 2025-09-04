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

public class cloudmailer extends ConsoleDemo {
  static CloudMailer cloudmailer1 = new CloudMailer();

  public static void main(String[] args) {
    try {
      cloudmailer1.addCloudMailerEventListener(new DefaultCloudMailerEventListener() {
        public void SSLServerAuthentication(CloudMailerSSLServerAuthenticationEvent e) {
          e.accept = true;
        }
      });

      int servicePrompt = Character.getNumericValue(ask("Which mail service provider would you like to use to send your message", "?", "\n  [0] - Amazon SES\n  [1] - Gmail\n  [2] - Office365\n"));

      // Prompt for authentication information.
      if (servicePrompt == 0) {
        cloudmailer1.setServiceProvider(0);
        cloudmailer1.getAccount().setAmazonAccessKey(prompt("Enter your Amazon access key", ":"));
        cloudmailer1.getAccount().setAmazonSecretKey(prompt("Enter your Amazon secret key", ":"));
      } else if (servicePrompt == 1) {
        cloudmailer1.setServiceProvider(1);
        cloudmailer1.getOAuth().setServerAuthURL("https://accounts.google.com/o/oauth2/auth");
        cloudmailer1.getOAuth().setServerTokenURL("https://accounts.google.com/o/oauth2/token");
        cloudmailer1.getOAuth().setClientId(prompt("Enter your OAuth client ID", ":"));
        cloudmailer1.getOAuth().setClientSecret(prompt("Enter your OAuth client secret", ":"));
        cloudmailer1.getOAuth().setAuthorizationScope(prompt("Enter desired authorization scopes (leave a space between each one if providing multiple)", ":"));
        System.out.print("Authenticating... ");
        cloudmailer1.authorize();
        System.out.println(" done.");
      } else if (servicePrompt == 2) {
        cloudmailer1.setServiceProvider(2);
        cloudmailer1.getOAuth().setServerAuthURL("https://login.microsoftonline.com/common/oauth2/v2.0/authorize");
        cloudmailer1.getOAuth().setServerTokenURL("https://login.microsoftonline.com/common/oauth2/v2.0/token");
        cloudmailer1.getOAuth().setClientId(prompt("Enter your OAuth client ID", ":"));
        cloudmailer1.getOAuth().setClientSecret(prompt("Enter your OAuth client secret", ":"));
        cloudmailer1.getOAuth().setAuthorizationScope(prompt("Enter desired authorization scopes (leave a space between each one if providing multiple)", ":"));
        System.out.print("Authenticating... ");
        cloudmailer1.authorize();
        System.out.println(" done.");
      } else {
        throw new Exception("Invalid mail service provider.");
      }

      // Prompt for message information.
      cloudmailer1.setSendTo(prompt("Provide email address(es) to receive message (leave a comma between each one if providing multiple)", ":"));
      cloudmailer1.setFrom(prompt("Provide email address that is sending message", ":"));
      cloudmailer1.setSubject(prompt("Provide message subject (optional)", ":"));
      cloudmailer1.setMessageHTML(prompt("Provide HTML message text", ":"));
      int attachmentPrompt = Character.getNumericValue(ask("Would you like to add an attachment", "?", "\n  [0] - No\n  [1] - Yes\n"));
      if (attachmentPrompt == 1) {
        String attachmentPath = prompt("Provide full path of attachment to add", ":");
        if (!attachmentPath.isEmpty()) {
          cloudmailer1.addAttachment(attachmentPath);
        }
      }

      // Send email.
      System.out.print("Sending message... ");
      cloudmailer1.send();
      System.out.println(" sent.");
    } catch (Exception e) {
      displayError(e);
    }
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



