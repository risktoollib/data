const { firefox } = require("playwright");

(async () => {
  const browser = await firefox.launch({ headless: true });
  const context = await browser.newContext();
  const page = await context.newPage();
  let jsonDataExtracted = false;

  // Enable request interception
  await page.route("**/*", (route) => {
    const request = route.request();
    if (
      request.url() ===
      "https://smartbuy.bvdpetroleum.com/api/getFuelSites?filter_result=no"
    ) {
      request.response().then((response) => {
        response.json().then((jsonData) => {
          console.log("Extracted JSON Data:", jsonData);
          // Save the JSON data to a file named petrocan.json
          const fs = require("fs");
          fs.writeFile(
            "cardlocks-petrocan.json",
            JSON.stringify(jsonData, null, 2),
            (err) => {
              if (err) {
                console.error("Error writing file:", err);
              } else {
                console.log("JSON data saved as cardlocks-petrocan.json");
                jsonDataExtracted = true;
              }
            }
          );
        });
      });
      route.continue();
    } else {
      route.continue();
    }
  });

  // Navigate and wait for network to be idle
  await page.goto("https://bvdgroup.com/plan-your-route", {
    waitUntil: "networkidle",
  });

  // Additional check to ensure the file has been written before closing the browser
  const checkDataExtraction = () => {
    if (jsonDataExtracted) {
      browser.close();
    } else {
      setTimeout(checkDataExtraction, 1000); // Check every second
    }
  };

  checkDataExtraction();
})();
