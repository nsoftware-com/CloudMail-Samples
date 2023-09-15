/*
 * Cloud Mail 2022 .NET Edition - Sample Project
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

using System.Collections.Generic;
﻿using System;
using System.Threading.Tasks;
using nsoftware.async.CloudMail;

class gmailDemo
{
  private static Gmail gmail = new nsoftware.async.CloudMail.Gmail();
  private static bool listingEmails = false;

  static async Task Main(string[] args)
  {
    try
    {
      gmail.OnMessageInfo += gmail_OnMessageInfo;

      // Prompt for authentication information.
      Console.Write("Enter your OAuth client ID: ");
      gmail.OAuth.ClientId = Console.ReadLine();
      Console.Write("Enter your OAuth client secret: ");
      gmail.OAuth.ClientSecret = Console.ReadLine();

      gmail.OAuth.ServerAuthURL = "https://accounts.google.com/o/oauth2/auth";
      gmail.OAuth.ServerTokenURL = "https://accounts.google.com/o/oauth2/token";
      gmail.OAuth.AuthorizationScope = "https://mail.google.com/";

      await gmail.Authorize();

      // Process user commands.
      Console.WriteLine("Type \"?\" or \"help\" for a list of commands.");
      Console.Write("gmail> ");
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
          Console.WriteLine("  ls                     list the next 100 messages");
          Console.WriteLine("  view <message id>      view the content of the selected message");
          Console.WriteLine("  compose                compose a message");
          Console.WriteLine("  quit                   exit the application");
        }
        else if (arguments[0] == "ls")
        {
          listingEmails = true;
          await gmail.Config("ResponseType=4");
          await gmail.ListMessages("", "");

        }
        else if (arguments[0] == "view")
        {
          if (arguments.Length > 1)
          {
            listingEmails = false;
            await gmail.Config("ResponseType=0");
            await gmail.FetchMessageRaw(arguments[1]);
            Console.WriteLine(gmail.Message);
          }
        }
        else if (arguments[0] == "compose")
        {
          // Prompt for message information.
          Console.Write("Provide email address(es) to receive message (leave a comma between each one if providing multiple): ");
          gmail.MessageTo = Console.ReadLine();

          Console.Write("Provide message subject (optional): ");
          gmail.MessageSubject = Console.ReadLine();

          Console.Write("Provide message body type (text/html): ");
          gmail.MessageBodyContentType = Console.ReadLine();

          Console.Write("Provide message text: ");
          gmail.MessageBodyContent = Console.ReadLine();

          Console.WriteLine("Would you like to add an attachment?");
          Console.WriteLine("   [0] - No");
          Console.WriteLine("   [1] - Yes");
          int attachmentPrompt = int.Parse(Console.ReadLine());
          if (attachmentPrompt == 1)
          {
            Console.Write("Provide full path of attachment to add: ");
            string attachmentPath = Console.ReadLine();
            if (!string.IsNullOrEmpty(attachmentPath)) gmail.MessageAttachments = attachmentPath;
          }

          // Send email.
          Console.Write("Sending message... ");
          await gmail.SendMail();
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

        Console.Write("gmail> ");
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void gmail_OnMessageInfo(object sender, GmailMessageInfoEventArgs e)
  {
    if (listingEmails)
    {
      Console.WriteLine(e.Id);
    }
  }
}


class ConsoleDemo
{
  public static Dictionary<string, string> ParseArgs(string[] args)
  {
    Dictionary<string, string> dict = new Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // If it starts with a "/" check the next argument.
      // If the next argument does NOT start with a "/" then this is paired, and the next argument is the value.
      // Otherwise, the next argument starts with a "/" and the current argument is a switch.

      // If it doesn't start with a "/" then it's not paired and we assume it's a standalone argument.

      if (args[i].StartsWith("/"))
      {
        // Either a paired argument or a switch.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Paired argument.
          dict.Add(args[i].TrimStart('/'), args[i + 1]);
          // Skip the value in the next iteration.
          i++;
        }
        else
        {
          // Switch, no value.
          dict.Add(args[i].TrimStart('/'), "");
        }
      }
      else
      {
        // Standalone argument. The argument is the value, use the index as a key.
        dict.Add(i.ToString(), args[i]);
      }
    }
    return dict;
  }

  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}