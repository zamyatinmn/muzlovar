// Browser artwork lifecycle against an isolated filesystem store.
import { spawn, spawnSync } from 'node:child_process';
import { existsSync, mkdtempSync, mkdirSync, readFileSync, readdirSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import net from 'node:net';
import { fileURLToPath } from 'node:url';
import assert from 'node:assert/strict';
import { chromium } from 'playwright-core';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
function chromePath() {
  if (process.env.MUZLOVAR_E2E_CHROME) return process.env.MUZLOVAR_E2E_CHROME;
  const base = path.join(process.env.LOCALAPPDATA || '', 'ms-playwright');
  for (const folder of readdirSync(base)) {
    if (!folder.startsWith('chromium')) continue;
    for (const sub of ['chrome-win64', 'chrome-win']) {
      const exe = path.join(base, folder, sub, folder.startsWith('chromium_headless') ? 'headless_shell.exe' : 'chrome.exe');
      if (existsSync(exe)) return exe;
    }
  }
  throw new Error('Set MUZLOVAR_E2E_CHROME to Chrome/Chromium');
}
if (!process.env.MUZLOVAR_E2E_SKIP_BUILD) {
  const built = spawnSync('cabal', ['build', 'exe:muzlovar'], {cwd:root, stdio:'inherit'});
  assert.equal(built.status, 0);
}
const listed = spawnSync('cabal', ['list-bin', 'exe:muzlovar'], {cwd:root, encoding:'utf8'});
assert.equal(listed.status, 0);
const store = mkdtempSync(path.join(tmpdir(), 'muzlovar-artwork-'));
for (const dir of ['rules', 'playlists', 'trash']) mkdirSync(path.join(store, dir));
const port = await new Promise(resolve => {
  const listener = net.createServer();
  listener.listen(0, '127.0.0.1', () => {
    const chosen = listener.address().port;
    listener.close(() => resolve(chosen));
  });
});
const base = 'http://127.0.0.1:' + port;
const server = spawn(listed.stdout.trim(), [], {cwd:root, env:{...process.env,
  MUZLOVAR_PORT:String(port), MUZLOVAR_HOST:'127.0.0.1', MUZLOVAR_USERNAME:'', MUZLOVAR_PASSWORD:'',
  MUZLOVAR_RULES_DIR:path.join(store, 'rules'), MUZLOVAR_PLAYLISTS_DIR:path.join(store, 'playlists'),
  MUZLOVAR_TRASH_DIR:path.join(store, 'trash')}, stdio:'ignore'});
let browser;
try {
  let started = false;
  for (let i=0; i<120; i++) {
    try { if ((await fetch(base + '/health')).ok) { started = true; break; } } catch (_) {}
    await new Promise(resolve => setTimeout(resolve, 250));
  }
  assert(started, 'server did not start');
  browser = await chromium.launch({executablePath:chromePath(), headless:true});
  const context = await browser.newContext({locale:'ru-RU'});
  const page = await context.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const fixture = ext => path.join(root, 'test/artwork/sample.' + ext);
  const artwork = (slug, ext) => path.join(store, 'playlists', slug + '.' + ext);
  async function valid() { await page.waitForSelector('#e-validity.ok'); }
  async function preview() {
    await page.waitForFunction(() => {
      const image = document.getElementById('e-artwork-image');
      const card = document.getElementById('pl-artwork-image');
      return !image.hidden && image.complete && image.naturalWidth > 0
        && !card.hidden && card.complete && card.naturalWidth > 0 && card.src === image.src;
    });
  }

  const endpoint = base + '/api/playlists/artwork-e2e/artwork';
  const artworkResponses = method => page.waitForResponse(r => new URL(r.url()).pathname.endsWith('/artwork') && r.request().method() === method);
  const list = await context.newPage();
  let artworkRequests = 0;
  page.on('request', request => { if (new URL(request.url()).pathname.endsWith('/artwork') && ['PUT','DELETE'].includes(request.method())) artworkRequests++; });
  async function choose(ext) {
    const done = artworkResponses('PUT');
    const picker = page.waitForEvent('filechooser');
    await page.click('#e-artwork-picker');
    await (await picker).setFiles(fixture(ext));
    assert.equal((await done).status(), 200);
    await preview();
    await page.waitForFunction(() => !document.getElementById('e-artwork-picker').disabled);
    assert.deepEqual(readFileSync(artwork('artwork-e2e', ext)), readFileSync(fixture(ext)));
  }
  async function publish() {
    const response = page.waitForResponse(r => r.request().method() !== 'GET' && new URL(r.url()).pathname.startsWith('/api/playlists') && !new URL(r.url()).pathname.endsWith('/artwork'));
    await page.click('#e-publish');
    const first = await response;
    if (first.status() === 409) {
      await page.waitForSelector('#overwrite-dialog[open]');
      const retried = page.waitForResponse(r => r.request().method() === 'PUT' && r.url().includes('overwrite=1'));
      await page.click('#overwrite-dialog .primary');
      assert.equal((await retried).status(), 200);
    } else assert([200,201].includes(first.status()));
  }

  await page.goto(base + '/new', {waitUntil:'networkidle'});
  await page.waitForSelector('.ing[data-chip-field="год"]');
  assert(await page.locator('#e-artwork-picker').isEnabled(), 'new playlists allow local artwork selection');
  assert.equal(await page.locator('#e-artwork-save').count(), 0);
  await page.fill('#e-name', 'Artwork E2E');
  await page.click('.ing[data-chip-field="год"]');
  await valid();
  await publish();
  await page.waitForFunction(() => !document.getElementById('e-artwork-picker').disabled);
  assert.equal(artworkRequests, 0, 'publishing must not upload/delete artwork');
  const square = await page.locator('.artwork-preview').boundingBox();
  assert(Math.abs(square.width - square.height) < 1);
  assert(await page.locator('#e-artwork-placeholder').isVisible());
  assert(await page.locator('#pl-artwork-image').isHidden());
  await list.goto(base + '/', {waitUntil:'networkidle'});
  assert.equal(await list.locator('.artwork-cell img').count(), 0);
  await choose('jpg');
  await list.waitForFunction(() => { const image = document.querySelector('.artwork-cell img'); return image && image.complete && image.naturalWidth > 0; });
  assert(await page.locator('#e-artwork-message').isHidden());
  assert.equal(await page.locator('#e-artwork-image').evaluate(image => getComputedStyle(image).objectFit), 'cover');
  console.log('  ✓ immediate JPEG upload updates both previews and the open list; no save button');

  await page.hover('#e-artwork-picker');
  await page.waitForFunction(() => getComputedStyle(document.querySelector('.artwork-overlay')).opacity === '1');
  assert.equal(await page.locator('.artwork-overlay').textContent(), 'Изменить');
  const previous = await page.locator('#e-artwork-image').getAttribute('src');
  let release;
  const gate = new Promise(resolve => release = resolve);
  await page.route(endpoint, async route => {
    if (route.request().method() !== 'PUT') return route.continue();
    await gate;
    await route.fulfill({status:500, contentType:'application/json', body:JSON.stringify({error:{message:'Injected upload failure'}})});
  });
  const failure = artworkResponses('PUT');
  await page.setInputFiles('#e-artwork-file', fixture('png'));
  await page.waitForSelector('#e-artwork-loading:not([hidden])');
  assert(await page.locator('#e-artwork-picker').isDisabled());
  assert(await page.locator('#e-artwork-remove').isDisabled());
  assert.equal(await page.locator('#e-artwork-image').getAttribute('src'), previous);
  const requestsBefore = artworkRequests;
  await page.setInputFiles('#e-artwork-file', fixture('webp'));
  assert.equal(artworkRequests, requestsBefore, 'busy artwork must block repeated operation');
  release();
  assert.equal((await failure).status(), 500);
  await page.waitForSelector('#e-artwork-message:not([hidden])');
  assert.equal(await page.locator('#e-artwork-image').getAttribute('src'), previous);
  assert.equal(await page.locator('#pl-artwork-image').getAttribute('src'), previous);
  assert.deepEqual(readFileSync(artwork('artwork-e2e','jpg')), readFileSync(fixture('jpg')));
  await page.unroute(endpoint);
  console.log('  ✓ hover, loading, repeated-operation lock, failed upload preserves the old cover');

  await choose('png');
  assert(!existsSync(artwork('artwork-e2e','jpg')));
  await choose('webp');
  assert(!existsSync(artwork('artwork-e2e','png')));
  await choose('gif');
  assert(!existsSync(artwork('artwork-e2e','webp')));
  await page.reload({waitUntil:'networkidle'});
  await preview();
  console.log('  ✓ immediate PNG/WebP/GIF replacement, original bytes, persisted preview');

  const bad = artworkResponses('PUT');
  const priorGif = await page.locator('#e-artwork-image').getAttribute('src');
  await page.setInputFiles('#e-artwork-file', {name:'fake.png', mimeType:'image/png', buffer:Buffer.from('not an image')});
  assert.equal((await bad).status(), 415);
  await page.waitForSelector('#e-artwork-message:not([hidden])');
  assert.equal(await page.locator('#e-artwork-image').getAttribute('src'), priorGif);
  console.log('  ✓ actual upload validation rejects spoofed image and preserves GIF');

  await page.route(endpoint, route => route.request().method() === 'DELETE'
    ? route.fulfill({status:500, contentType:'application/json', body:JSON.stringify({error:{message:'Injected delete failure'}})})
    : route.continue());
  const failedDelete = artworkResponses('DELETE');
  await page.click('#e-artwork-remove');
  assert.equal((await failedDelete).status(), 500);
  await page.waitForFunction(() => !document.getElementById('e-artwork-remove').disabled);
  assert.equal(await page.locator('#e-artwork-image').getAttribute('src'), priorGif);
  await page.unroute(endpoint);
  let pickerEvents = 0;
  page.on('filechooser', () => pickerEvents++);
  await page.evaluate(() => {
    window.artworkDeleteBubbled = false;
    document.querySelector('.artwork-preview').addEventListener('click', () => window.artworkDeleteBubbled = true);
  });
  const deleted = artworkResponses('DELETE');
  await page.click('#e-artwork-remove');
  assert.equal((await deleted).status(), 200);
  await page.waitForSelector('#e-artwork-placeholder:not([hidden])');
  assert(await page.locator('#pl-artwork-image').isHidden());
  assert(!existsSync(artwork('artwork-e2e','gif')));
  assert.equal(pickerEvents, 0);
  assert.equal(await page.evaluate(() => window.artworkDeleteBubbled), false);
  await list.waitForFunction(() => document.querySelector('.artwork-cell').children.length === 0);
  console.log('  ✓ immediate delete, failure recovery, stopPropagation, no picker, live empty list cell');

  await choose('gif');
  const beforePublish = artworkRequests;
  await page.fill('#e-name', 'Artwork Renamed');
  await valid();
  await publish();
  await preview();
  assert.equal(artworkRequests, beforePublish, 'recipe save must not call artwork API');
  assert(!existsSync(artwork('artwork-e2e','gif')));
  assert.deepEqual(readFileSync(artwork('artwork-renamed','gif')), readFileSync(fixture('gif')));
  console.log('  ✓ recipe rename preserves artwork without issuing any artwork mutation');

  writeFileSync(path.join(store, 'playlists/layout.nsp'), JSON.stringify({name:'A very long playlist name '.repeat(8), all:[{is:{year:2020}}]}));
  const geometry = () => list.evaluate(() => {
    const row = document.querySelector('tr[data-playlist-slug="layout"]');
    const cells = [...row.children].map(cell => { const r=cell.getBoundingClientRect(); return {left:r.left,width:r.width}; });
    const text = row.querySelector('.playlist-title-text');
    const thumbnail = row.querySelector('.list-artwork');
    const artCell = row.querySelector('.artwork-cell').getBoundingClientRect();
    const allTitleX = [...document.querySelectorAll('.playlist-title-text')].map(node => node.getBoundingClientRect().left);
    const r = thumbnail && thumbnail.getBoundingClientRect();
    return {cells, allTitleX, firstHeading:document.querySelector('thead th').textContent, headings:document.querySelectorAll('thead th').length,
      overflow:text.scrollWidth>text.clientWidth, ellipsis:getComputedStyle(text).textOverflow,
      thumbnail:r && {width:r.width,height:r.height,center:r.left+r.width/2,cellCenter:artCell.left+artCell.width/2,objectFit:getComputedStyle(thumbnail).objectFit}};
  });
  for (const width of [1440,900,700]) {
    await list.setViewportSize({width,height:900});
    await list.goto(base+'/', {waitUntil:'networkidle'});
    const before = await geometry();
    assert.equal(before.thumbnail,null);
    const uploaded = await fetch(base+'/api/playlists/layout/artwork', {method:'PUT',body:readFileSync(fixture('png'))});
    assert(uploaded.ok);
    await list.reload({waitUntil:'networkidle'});
    const after = await geometry();
    assert.equal(after.headings,8);
    assert.equal(after.firstHeading,'');
    assert(Math.abs(after.cells[0].width-58)<1,'artwork column must stay 58px');
    assert(after.allTitleX.every(x => Math.abs(x-after.allTitleX[0])<1),'titles must align with and without artwork');
    assert(after.overflow && after.ellipsis==='ellipsis');
    assert.equal(after.thumbnail.width,42);
    assert.equal(after.thumbnail.height,42);
    assert.equal(after.thumbnail.objectFit,'cover');
    assert(Math.abs(after.thumbnail.center-after.thumbnail.cellCenter)<1,'thumbnail must be centered');
    after.cells.forEach((cell,i) => {
      assert(Math.abs(cell.width-before.cells[i].width)<1,'cover changed column width');
      assert(Math.abs(cell.left-before.cells[i].left)<1,'cover displaced another column');
    });
    await fetch(base+'/api/playlists/layout/artwork', {method:'DELETE'});
  }
  console.log('  ✓ dedicated blank 58px column, 42px image, title alignment and stable layout at 1440/900/700px');

  const external=JSON.stringify({name:'External artwork',all:[{unknownop:{title:'x'}}]});
  writeFileSync(path.join(store,'playlists/external-artwork.nsp'),external);
  await page.goto(base+'/edit/external-artwork', {waitUntil:'networkidle'});
  await page.waitForFunction(() => document.getElementById('e-publish').disabled && !document.getElementById('e-artwork-picker').disabled);
  await page.selectOption('#ui-locale','en');
  assert.equal(await page.locator('.artwork-block .block-title').textContent(),'Artwork');
  const externalUploaded=artworkResponses('PUT');
  await page.setInputFiles('#e-artwork-file',fixture('webp'));
  assert.equal((await externalUploaded).status(),200);
  await preview();
  assert.equal(readFileSync(path.join(store,'playlists/external-artwork.nsp'),'utf8'),external);
  assert.deepEqual(readFileSync(artwork('external-artwork','webp')),readFileSync(fixture('webp')));
  console.log('  ✓ immediate artwork upload for read-only recipe, English UI');

  await page.goto(base+'/new', {waitUntil:'networkidle'});
  await page.waitForSelector('.ing[data-chip-field="год"]');
  await page.fill('#e-name','Draft Original');
  await page.click('.ing[data-chip-field="год"]');
  await valid();
  const beforeDraft = artworkRequests;
  await page.setInputFiles('#e-artwork-file',fixture('png'));
  await preview();
  const draftPng = await page.locator('#e-artwork-image').getAttribute('src');
  assert(draftPng.startsWith('blob:'));
  await page.setInputFiles('#e-artwork-file',fixture('gif'));
  await preview();
  assert.notEqual(await page.locator('#e-artwork-image').getAttribute('src'),draftPng);
  await page.click('#e-artwork-remove');
  assert(await page.locator('#e-artwork-placeholder').isVisible());
  assert(await page.locator('#pl-artwork-image').isHidden());
  assert.equal(artworkRequests,beforeDraft,'local choose/replace/delete must not call artwork API');
  console.log('  ✓ draft selection, immediate dual preview, replacement and local deletion');

  await page.setInputFiles('#e-artwork-file',fixture('webp'));
  await preview();
  const draftWebp = await page.locator('#e-artwork-image').getAttribute('src');
  await page.fill('#e-name','Draft Final');
  await valid();
  const recipes = base+'/api/playlists?*';
  await page.route(recipes,route => route.request().method()==='POST'
    ? route.fulfill({status:500,contentType:'application/json',body:JSON.stringify({error:{message:'Injected publication failure'}})})
    : route.continue());
  const failedPublish=page.waitForResponse(r => r.request().method()==='POST' && new URL(r.url()).pathname==='/api/playlists');
  await page.click('#e-publish');
  assert.equal((await failedPublish).status(),500);
  await page.waitForFunction(() => !document.getElementById('e-artwork-picker').disabled);
  assert.equal(await page.locator('#e-artwork-image').getAttribute('src'),draftWebp);
  assert.equal(artworkRequests,beforeDraft);
  assert(!existsSync(artwork('draft-final','nsp')));
  await page.unroute(recipes);
  console.log('  ✓ failed first publication retains local artwork and issues no PUT');

  const draftEndpoint=base+'/api/playlists/draft-final/artwork';
  await page.route(draftEndpoint,route => route.request().method()==='PUT'
    ? route.fulfill({status:500,contentType:'application/json',body:JSON.stringify({error:{message:'Injected initial upload failure'}})})
    : route.continue());
  const initialUpload=artworkResponses('PUT');
  await publish();
  assert.equal((await initialUpload).status(),500);
  await page.waitForSelector('#e-artwork-retry:not([hidden]):not([disabled])');
  assert(existsSync(artwork('draft-final','nsp')));
  assert(!existsSync(artwork('draft-final','webp')));
  assert(!existsSync(artwork('draft-original','nsp')));
  assert.equal(await page.locator('#e-artwork-image').getAttribute('src'),draftWebp);
  assert.equal(await page.locator('#pl-artwork-image').getAttribute('src'),draftWebp);
  assert((await page.locator('#e-artwork-message').textContent()).includes('published'));
  const beforeRecipeSave=artworkRequests;
  await publish();
  await page.waitForFunction(() => !document.getElementById('e-artwork-picker').disabled);
  assert.equal(artworkRequests,beforeRecipeSave,'recipe saving must not retry artwork');
  await page.unroute(draftEndpoint);
  const retry=artworkResponses('PUT');
  await page.click('#e-artwork-retry');
  assert.equal((await retry).status(),200);
  await preview();
  assert.deepEqual(readFileSync(artwork('draft-final','webp')),readFileSync(fixture('webp')));
  assert(await page.locator('#e-artwork-retry').isHidden());
  console.log('  ✓ published recipe survives artwork failure, local preview persists and explicit retry uses final basename');

  await page.goto(base+'/new',{waitUntil:'networkidle'});
  await page.waitForSelector('.ing[data-chip-field="год"]');
  await page.fill('#e-name','Draft Success');
  await page.click('.ing[data-chip-field="год"]');
  await valid();
  await page.setInputFiles('#e-artwork-file',fixture('png'));
  await preview();
  const uploadedDraft=artworkResponses('PUT');
  await publish();
  assert.equal((await uploadedDraft).status(),200);
  await preview();
  assert(existsSync(artwork('draft-success','nsp')));
  assert.deepEqual(readFileSync(artwork('draft-success','png')),readFileSync(fixture('png')));
  await page.waitForFunction(() => !document.getElementById('e-artwork-picker').disabled);
  await page.fill('#e-name','Draft Duplicate');
  await valid();
  const duplicateCover=artworkResponses('PUT');
  await page.click('#e-publish-new');
  assert.equal((await duplicateCover).status(),200);
  await preview();
  assert(existsSync(artwork('draft-duplicate','nsp')));
  assert.deepEqual(readFileSync(artwork('draft-duplicate','png')),readFileSync(fixture('png')));
  assert.deepEqual(readFileSync(artwork('draft-success','png')),readFileSync(fixture('png')));
  console.log('  ✓ successful first publish and Save as new create matching sidecars without changing source bytes');

  writeFileSync(path.join(store,'playlists/broken-list.nsp'),'invalid json');
  writeFileSync(path.join(store,'rules/draft-only.mix'),readFileSync(path.join(store,'rules/draft-success.mix')));
  await list.goto(base+'/',{waitUntil:'networkidle'});
  await list.selectOption('#ui-locale','ru');
  const rowSelector='tr[data-playlist-slug="draft-duplicate"]';
  const deleteSelector=rowSelector+' .playlist-delete';
  assert.deepEqual(await list.locator('.playlist-list thead th').allTextContents(),
    ['', 'Название','Описание','Свойства','Сортировка','Дерево условий','Изменено','']);
  assert.equal(await list.locator('.playlist-list .badge.managed, .playlist-list .file, .playlist-list .actions').count(),0);
  assert.equal(await list.locator('.playlist-list a.btn').count(),0);
  assert(await list.locator('tr[data-playlist-slug="external-artwork"] .badge.external').isVisible());
  assert(await list.locator('tr[data-playlist-slug="broken-list"] .playlist-status.broken').isVisible());
  assert(await list.locator('tr[data-playlist-slug="draft-only"] .badge.draft').isVisible());
  await list.mouse.move(0,0);
  await list.waitForFunction(selector => getComputedStyle(document.querySelector(selector)).opacity==='0',deleteSelector);
  await list.hover(rowSelector);
  await list.waitForFunction(selector => getComputedStyle(document.querySelector(selector)).opacity==='1',deleteSelector);
  assert.equal(await list.locator(deleteSelector).getAttribute('aria-label'),'Удалить');
  assert.equal(await list.locator(deleteSelector).getAttribute('title'),'Удалить');
  assert.equal(await list.locator(deleteSelector+' svg').count(),1);
  await list.mouse.move(0,0);
  await list.locator(rowSelector).focus();
  await list.waitForFunction(selector => getComputedStyle(document.querySelector(selector)).opacity==='1',deleteSelector);
  await list.keyboard.press('Tab');
  assert(await list.locator(deleteSelector).evaluate(node => document.activeElement===node));
  console.log('  ✓ eight meaningful columns, attention statuses retained, hover and keyboard reveal SVG delete');

  await Promise.all([list.waitForURL('**/edit/draft-duplicate'),list.click(rowSelector+' .summary')]);
  for (const key of ['Enter','Space']) {
    await list.goto(base+'/',{waitUntil:'networkidle'});
    await list.locator(rowSelector).focus();
    await Promise.all([list.waitForURL('**/edit/draft-duplicate'),list.keyboard.press(key)]);
  }
  console.log('  ✓ row click, Enter and Space open the correct editor');

  await list.goto(base+'/',{waitUntil:'networkidle'});
  await list.locator(rowSelector).focus();
  await list.keyboard.press('Tab');
  await list.keyboard.press('Enter');
  await list.waitForSelector('#delete-dialog[open]');
  assert.equal(new URL(list.url()).pathname,'/','delete keyboard action must not navigate');
  assert(await list.locator('#delete-confirm').isDisabled());
  await list.locator('#delete-dialog .row button:first-child').click();
  await list.click(deleteSelector);
  await list.waitForSelector('#delete-dialog[open]');
  assert.equal(new URL(list.url()).pathname,'/','delete click must not navigate');
  await list.fill('#delete-input','incorrect');
  assert(await list.locator('#delete-confirm').isDisabled());
  await list.fill('#delete-input','Draft Duplicate');
  await Promise.all([list.waitForURL('**/trash'),list.click('#delete-confirm')]);
  assert(!existsSync(artwork('draft-duplicate','nsp')));
  assert(!existsSync(artwork('draft-duplicate','png')));
  const restored=list.waitForResponse(r => r.url().includes('/restore') && r.request().method()==='POST');
  await list.click('[data-restore]');
  assert.equal((await restored).status(),200);
  assert(existsSync(artwork('draft-duplicate','nsp')));
  assert.deepEqual(readFileSync(artwork('draft-duplicate','png')),readFileSync(fixture('png')));
  console.log('  ✓ mouse/keyboard delete never opens editor; exact-name confirmation and Trash restore preserved');
  assert.deepEqual(errors,[]);
  console.log('[e2e-artwork] ALL PASSED (15)');
} finally {
  if (browser) await browser.close();
  server.kill();
  await new Promise(resolve => server.once('exit', resolve));
  assert(store.startsWith(path.join(tmpdir(), 'muzlovar-artwork-')));
  rmSync(store, {recursive:true, force:true});
}
