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

class amazonsesDemo
{
  private static Amazonses amazonses = new nsoftware.async.CloudMail.Amazonses();

  static async Task Main(string[] args)
  {
    try
    {
      amazonses.OnSSLServerAuthentication += amazonses_OnSSLServerAuthentication;

      // Prompt for authentication information.
      Console.Write("Enter your access key: ");
      amazonses.AccessKey = Console.ReadLine();
      Console.Write("Enter your secret key: ");
      amazonses.SecretKey = Console.ReadLine();

      // Prompt for message information.
      Console.Write("Provide email address(es) to receive message (leave a comma between each one if providing multiple): ");
      amazonses.SendTo = Console.ReadLine();

      Console.Write("Provide email address that is sending message: ");
      amazonses.From = Console.ReadLine();

      Console.Write("Provide message subject (optional): ");
      amazonses.Subject = Console.ReadLine();

      Console.Write("Provide HTML message text: ");
      amazonses.MessageHTML = Console.ReadLine();

      Console.WriteLine("Would you like to add an attachment?");
      Console.WriteLine("   [0] - No");
      Console.WriteLine("   [1] - Yes");
      int attachmentPrompt = int.Parse(Console.ReadLine());
      if (attachmentPrompt == 1)
      {
        Console.Write("Provide full path of attachment to add: ");
        string attachmentPath = Console.ReadLine();
        if (!string.IsNullOrEmpty(attachmentPath)) await amazonses.AddAttachment(attachmentPath);
      }

      // Send email.
      Console.Write("Sending message... ");
      await amazonses.Send();
      Console.WriteLine(" sent.");
    }
    catch (Exception e)
    {
      Console.WriteLine(e.Message);
    }
  }
  private static void amazonses_OnSSLServerAuthentication(object sender, AmazonsesSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
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