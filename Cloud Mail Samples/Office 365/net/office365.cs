/*
 * Cloud Mail 2024 .NET Edition - Sample Project
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
 * 
 */

﻿using System;
using nsoftware.CloudMail;

class office365Demo
{
  private static Office365 office365 = new nsoftware.CloudMail.Office365();
  private static string currFolder = "";

  static void Main(string[] args)
  {
    try
    {
      // Prompt for authentication information.
      Console.Write("Enter your OAuth client ID: ");
      office365.OAuth.ClientId = Console.ReadLine();
      Console.Write("Enter your OAuth client secret: ");
      office365.OAuth.ClientSecret = Console.ReadLine();

      office365.OAuth.ServerAuthURL = "https://login.microsoftonline.com/common/oauth2/v2.0/authorize";
      office365.OAuth.ServerTokenURL = "https://login.microsoftonline.com/common/oauth2/v2.0/token";
      office365.OAuth.AuthorizationScope = "user.read mail.readwrite mail.send mailboxsettings.readwrite";

      office365.Authorize();

      // Process user commands.
      Console.WriteLine("Type \"?\" or \"help\" for a list of commands.");
      Console.Write("office365> ");
      string command;
      string[] arguments;

      while (true)
      {
        command = Console.ReadLine();
        arguments = command.Split();

        if (arguments[0] == "?" || arguments[0] == "help")
        {
          Console.WriteLine("Commands: ");
          Console.WriteLine("  ?                      display the list of valid commands");
          Console.WriteLine("  help                   display the list of valid commands");
          Console.WriteLine("  ls                     list folders");
          Console.WriteLine("  select <folder>        select a subfolder");
          Console.WriteLine("  lm                     list messages");
          Console.WriteLine("  view <message id>      view the content of a listed message");
          Console.WriteLine("  compose                compose and send a message");
          Console.WriteLine("  quit                   exit the application");
        }
        else if (arguments[0] == "ls")
        {
          office365.ListFolders(currFolder); // Lists the root child folders.

          foreach (OLFolder folder in office365.Folders)
          {
            Console.WriteLine("  " + folder.DisplayName);
          }
        }
        else if (arguments[0] == "select")
        {
          if (arguments.Length > 1)
          {
            // Get the folder ID.
            bool found = false;
            office365.ListFolders(currFolder); // Lists the root child folders.

            foreach (OLFolder folder in office365.Folders)
            {
              if (folder.DisplayName == arguments[1])
              {
                currFolder = folder.Id;
                found = true;
                break;
              }
            }
            if (!found)
            {
              Console.WriteLine("No matching subfolder. Setting the current folder to the root folder.");
              currFolder = "";
            }
          }
        }
        else if (arguments[0] == "lm")
        {
          office365.Config("MessagePageSize=15");
          office365.ListMessages(currFolder, "");

          int i = 0;
          while (true)
          {
            if (i < office365.MessageInfo.Count)
            {
              Console.WriteLine("  " + i + ": " + office365.MessageInfo[i].Subject);
            }
            else
            {
              Console.Write("List additional messages? (y/n) ");
              if (Console.ReadLine() == "y") office365.ListMessages(currFolder, "");
              else break;
            }
            i++;
          }
        }
        else if (arguments[0] == "view")
        {
          if (arguments.Length > 1)
          {
            OLMessageInfo messageInfo = office365.MessageInfo[int.Parse(arguments[1])];
            Console.WriteLine("Subject: " + messageInfo.Subject);
            Console.WriteLine("From: " + messageInfo.From);
            Console.WriteLine("Date: " + messageInfo.ReceivedDate);
            Console.WriteLine("Content: " + messageInfo.BodyContent);
          }
        }
        else if (arguments[0] == "compose")
        {
          // Prompt for message information.
          Console.Write("Provide email address(es) to receive message (leave a comma between each one if providing multiple): ");
          office365.MessageTo = Console.ReadLine();

          Console.Write("Provide message subject (optional): ");
          office365.MessageSubject = Console.ReadLine();

          office365.MessageBodyContentType = "text";

          Console.Write("Provide message text: ");
          office365.MessageBodyContent = Console.ReadLine();

          // Send email.
          Console.Write("Sending message... ");
          office365.SendMail(true);
          Console.WriteLine(" sent.");
        }
        else if (arguments[0] == "quit")
        {
          break;
        }
        else if (arguments[0] == "")
        {
          // Do nothing.
        }
        else
        {
          Console.WriteLine("Invalid command.");
        } // End of command checking.

        Console.Write("office365> ");
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }
}




class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add a key to the dictionary for each argument.
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/", then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}