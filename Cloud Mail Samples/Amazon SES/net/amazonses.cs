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

class amazonsesDemo
{
  private static AmazonSES amazonses = new nsoftware.CloudMail.AmazonSES();

  static void Main(string[] args)
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
        if (!string.IsNullOrEmpty(attachmentPath)) amazonses.AddAttachment(attachmentPath);
      }

      // Send email.
      Console.Write("Sending message... ");
      amazonses.Send();
      Console.WriteLine(" sent.");
    }
    catch (Exception e)
    {
      Console.WriteLine(e.Message);
    }
  }
  private static void amazonses_OnSSLServerAuthentication(object sender, AmazonSESSSLServerAuthenticationEventArgs e)
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