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

class cloudmailerDemo
{
  private static CloudMailer cloudmailer;

  private static void cloudmailer_OnSSLServerAuthentication(object sender, CloudMailerSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  static void Main(string[] args)
  {
    cloudmailer = new CloudMailer();

    try
    {
      cloudmailer.OnSSLServerAuthentication += cloudmailer_OnSSLServerAuthentication;

      Console.WriteLine("Which mail service provider would you like to use to send your message?");
      Console.WriteLine("   [0] - Amazon SES");
      Console.WriteLine("   [1] - Gmail");
      Console.WriteLine("   [2] - Office 365");

      int servicePrompt = int.Parse(Console.ReadLine());

      // Prompt for authentication information.
      if (servicePrompt == 0)
      {
        cloudmailer.ServiceProvider = CloudMailerServiceProviders.spAmazonSES;

        Console.Write("Enter your Amazon access key:  ");
        cloudmailer.Account.AmazonAccessKey = Console.ReadLine();

        Console.Write("Enter your Amazon secret key:  ");
        cloudmailer.Account.AmazonSecretKey = Console.ReadLine();
      }
      else if (servicePrompt == 1)
      {
        cloudmailer.ServiceProvider = CloudMailerServiceProviders.spGmail;
        cloudmailer.OAuth.ServerAuthURL = "https://accounts.google.com/o/oauth2/auth";
        cloudmailer.OAuth.ServerTokenURL = "https://accounts.google.com/o/oauth2/token";

        Console.Write("Enter your OAuth client ID:  ");
        cloudmailer.OAuth.ClientId = Console.ReadLine();

        Console.Write("Enter your OAuth client secret:  ");
        cloudmailer.OAuth.ClientSecret = Console.ReadLine();

        Console.Write("Enter desired authorization scopes (leave a space between each one if providing multiple):  ");
        cloudmailer.OAuth.AuthorizationScope = Console.ReadLine();

        Console.Write("Authenticating... ");
        cloudmailer.Authorize();
        Console.WriteLine(" done.");
      }
      else if (servicePrompt == 2)
      {
        cloudmailer.ServiceProvider = CloudMailerServiceProviders.spOffice365;
        cloudmailer.OAuth.ServerAuthURL = "https://login.microsoftonline.com/common/oauth2/v2.0/authorize";
        cloudmailer.OAuth.ServerTokenURL = "https://login.microsoftonline.com/common/oauth2/v2.0/token";

        Console.Write("Enter your OAuth client ID:  ");
        cloudmailer.OAuth.ClientId = Console.ReadLine();

        Console.Write("Enter your OAuth client secret:  ");
        cloudmailer.OAuth.ClientSecret = Console.ReadLine();

        Console.Write("Enter desired authorization scopes (leave a space between each one if providing multiple):  ");
        cloudmailer.OAuth.AuthorizationScope = Console.ReadLine();

        Console.Write("Authenticating... ");
        cloudmailer.Authorize();
        Console.WriteLine(" done.");
      }
      else
      {
        throw new Exception("\nInvalid mail service provider.\n");
      }

      // Prompt for message information.
      Console.Write("Provide email address(es) to receive message (leave a comma between each one if providing multiple):  ");
      cloudmailer.SendTo = Console.ReadLine();

      Console.Write("Provide email address that is sending message:  ");
      cloudmailer.From = Console.ReadLine();

      Console.Write("Provide message subject (optional):  ");
      cloudmailer.Subject = Console.ReadLine();

      Console.Write("Provide HTML message text:  ");
      cloudmailer.MessageHTML = Console.ReadLine();

      Console.WriteLine("Would you like to add an attachment?");
      Console.WriteLine("   [0] - No");
      Console.WriteLine("   [1] - Yes");
      int attachmentPrompt = int.Parse(Console.ReadLine());
      if (attachmentPrompt == 1)
      {
        Console.Write("Provide full path of attachment to add: ");
        string attachmentPath = Console.ReadLine();
        if (!string.IsNullOrEmpty(attachmentPath)) cloudmailer.AddAttachment(attachmentPath);
      }

      // Send email.
      Console.Write("Sending message... ");
      cloudmailer.Send();
      Console.WriteLine(" sent.");
    }
    catch (Exception e)
    {
      Console.WriteLine(e.Message);
    }

    Console.WriteLine("\nPress any key to exit...");
    Console.ReadKey();
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