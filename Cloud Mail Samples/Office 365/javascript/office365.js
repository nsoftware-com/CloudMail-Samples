/*
 * Cloud Mail 2024 JavaScript Edition - Sample Project
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
 
const readline = require("readline");
const cloudmail = require("@nsoftware/cloudmail");

if(!cloudmail) {
  console.error("Cannot find cloudmail.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

let office365 = new cloudmail.office365();
let currFolder = "";
let lastPrompt = "";
let lastDefault = "";
let isListing = false;

main();

async function main() {
  // Set up event handlers.
  office365.on("FolderList", (e) => {
    if (isListing)
      console.log(e.displayName);
  });

  office365.on("MessageList", (e) => {
    if (isListing)
      console.log(e.id);
  });

  // Prompt for OAuth.
  prompt("clientId", "Enter your OAuth client ID", ":", "");
  
  rl.on("line", async (line) => {
    switch (lastPrompt) {
      case "clientId":
        office365.getOAuth().setClientId(line.trim());
        prompt("clientSecret", "Enter your OAuth client secret", ":", "");
        break;
      case "clientSecret":
        office365.getOAuth().setClientSecret(line.trim());
        office365.getOAuth().setServerAuthURL("https://login.microsoftonline.com/common/oauth2/v2.0/authorize");
        office365.getOAuth().setServerTokenURL("https://login.microsoftonline.com/common/oauth2/v2.0/token");
        office365.getOAuth().setAuthorizationScope("user.read mail.readwrite mail.send mailboxsettings.readwrite");
        console.log("\nAuthorizing...");
        try {
          await office365.authorize();
          console.log("Authorization successful.\n");
          console.log("Welcome to the Cloud Mail Office365 Demo.");
          console.log("Follow the prompts to view and send emails using the component.");
          console.log("------------------------------------------------------------\n");
          displayMenu();
          menuPrompt();
        } catch (e) {
          displayError(e);
        }
        break;
      case "selectFolder":
        currFolder = line.trim();
        console.log("Folder selected:", currFolder || "(root)");
        lastPrompt = "";
        menuPrompt();
        break;
      case "chooseMessage":
        try {
          await office365.retrieveMessage(line.trim());
          const msg = office365.getMessageInfo()?.collection?.elementData;
          console.log("Subject:", msg[0].m_Subject);
          console.log("From:", msg[0].m_From);
          console.log("Date:", msg[0].m_ReceivedDate);
          console.log("Content:", msg[0].m_BodyContent);
          lastPrompt = "";
        } catch (e) {
          displayError(e);
        }
        menuPrompt();
        break;
      case "composeTo":
        office365.setMessageTo(line.trim());
        prompt("composeSubject", "Subject", ":", "");
        break;
      case "composeSubject":
        office365.setMessageSubject(line.trim());
        prompt("composeBody", "Message Body", ":", "");
        break;
      case "composeBody":
        office365.setMessageBodyContentType("text");
        office365.setMessageBodyContent(line.trim());
        console.log("\nSending message...");
        try {
          await office365.sendMail(true);
          console.log("Message sent successfully!");
        } catch (e) {
          displayError(e);
        }
        lastPrompt = "";
        menuPrompt();
        break;
      default:
        // Handle menu commands.
        switch (line.trim()) {
          case "ls":
            isListing = true;
            console.log("Listing folders...");
            await office365.listFolders(currFolder);
            isListing = false;
            menuPrompt();
            break;
          case "select":
            prompt("selectFolder", "Enter folder ID (or nothing to list the root)", ":", "");
            break;
          case "lm":
            isListing = true;
            office365.config("MessagePageSize=15");
            console.log("Listing messages in folder:", currFolder || "root");
            await office365.listMessages(currFolder, "");
            isListing = false;
            menuPrompt();
            break;
          case "view":
            prompt("chooseMessage", "Enter message ID to view", ":", "");
            break;
          case "compose":
            prompt("composeTo", "Send to", ":", "");
            break;
          case "?":
            displayMenu();
            menuPrompt();
            break;
          case "quit":
          case "q":
            rl.close();
            process.exit(0);
            break;
          default:
            displayMenu();
            menuPrompt();
            break;
        }
        break;
    }
  });
}

function displayError(e) {
  console.error("\nError:", e.message || e);
  console.trace();
}

function displayMenu() {
  console.log("\nOffice365 Commands:");
  console.log("ls                  list folders");
  console.log("select              select a folder");
  console.log("lm                  list messages");
  console.log("view                view message content");
  console.log("compose             compose a new message");
  console.log("?                   display menu options");
  console.log("q or quit           quit\n");
}

function menuPrompt() {
  process.stdout.write("office365> ");
  lastPrompt = "";
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}]${punctuation} `);
}
