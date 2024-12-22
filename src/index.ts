// Copyright (C) 2023 Alvaro Ramirez https://xenodium.com
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

interface State {
  error?: string;
  filterByTags: string[];
  orgDocument: OrgDocument;
}

export function render(state: State, store: StateStore): RenderResult {
  let handlers: Array<{ nodeId: string, listenerName: string, handler: Function }> = [];
  const filteredHeadings = state.orgDocument.headings.filter(heading =>
    state.filterByTags.length === 0 || state.filterByTags.every(filterTag =>
      heading.tags?.map(tag => tag.toLowerCase()).includes(filterTag)
    )
  );

  const randomPick = new RandomPickRenderer(store).render(filteredHeadings);
  handlers = handlers.concat(randomPick.handlers);

  const tagPicker = new TagsPickerRenderer(store).render(state.orgDocument.headings);
  handlers = handlers.concat(tagPicker.handlers);

  const filterByTags = new FilterByTagsRenderer(store).render(state.filterByTags);
  handlers = handlers.concat(filterByTags.handlers);

  const videoList = new VideoListRenderer(store).render(filteredHeadings);
  handlers = handlers.concat(videoList.handlers);

  if (state.orgDocument.headings.length === 0) {
    return {
      html: '',
      handlers: []
    }
  }

  let html = `
<h1>🦬 emacs.tv</h1>
  ${randomPick.html}
${state.orgDocument.headings.length != 0 ?
`
<h2>Videos (${filteredHeadings.length})</h2>
filter by ${tagPicker.html} ${filterByTags.html}
<br>
<br>` : ''
}
${videoList.html}`;

  return {
    html: state.error ? state.error : html,
    handlers: handlers
  };
}

class RandomPickRenderer {
  private store: StateStore;

  constructor(store: StateStore) {
    this.store =store;
  }

  render(headings: OrgHeading[]): RenderResult {
    let handlers: Array<{ nodeId: string, listenerName: string, handler: Function }> = [];
    const renderableHeadings = headings.filter(heading =>
      heading.drawer?.MEDIA_URL || heading.drawer?.TOOBNIX_URL || heading.drawer?.YOUTUBE_URL
                                              );
    if (renderableHeadings.length === 0) {
      return {
        handlers: handlers,
        html: ''
      };
    }
    const randomIndex = Math.floor(Math.random() * renderableHeadings.length);
    const randomHeading = renderableHeadings[randomIndex];
    handlers.push({
      nodeId: 'die',
      listenerName: 'click',
      handler: () => this.store.refresh()
    });

    let player = '';
    if (randomHeading.drawer['MEDIA_URL']) {
      player = `
<video controls>
  <source src="${randomHeading.drawer['MEDIA_URL']}" type="video/webm">
  Your browser does not support the video tag.
</video>`;
  } else if (randomHeading.drawer['TOOBNIX_URL']) {
    const toobnixId = this.extractToobnixId(randomHeading.drawer['TOOBNIX_URL']);
    player = `
<div class="video-container">
  <iframe title="" src="https://toobnix.org/videos/embed/${toobnixId}"
          frameborder="0" allowfullscreen="" sandbox="allow-same-origin allow-scripts allow-popups allow-forms">
  </iframe>
</div>`;
  } else if (randomHeading.drawer['YOUTUBE_URL']) {
    const youtubeId = this.extractYouTubeId(randomHeading.drawer['YOUTUBE_URL']);
    player = `
<div class="video-container">
  <iframe src="https://www.youtube.com/embed/${youtubeId}"
          title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write;
          encrypted-media; gyroscope; picture-in-picture; web-share"
           referrerpolicy="strict-origin-when-cross-origin" allowfullscreen>
  </iframe>
</div>`;
  }
    return {
      handlers: handlers,
      html: `
<div>
  <h2 id="die">Lucky pick 🎲</h2>
  ${player}
  <div class="video--caption item">
    ${DateRenderer.render(randomHeading.drawer.DATE)}
    <p>
      <a href="${randomHeading.drawer['URL'] || randomHeading.drawer['YOUTUBE_URL'] || randomHeading.drawer['TOOBNIX_URL'] || randomHeading.drawer['MEDIA_URL']}">${randomHeading.title}</a> ${randomHeading.drawer['SPEAKERS'] ? ' (' + randomHeading.drawer['SPEAKERS'] + ')' : ''}
    </p>
  </div>
</div>`
    };
  }

  private extractYouTubeId(url: string): string {
    const match = url.match(/(?:https?:\/\/)?(?:www\.)?youtube\.com\/watch\?v=([^&]+)/) ||
      url.match(/youtu\.be\/([^?]+)/);
    return match ? match[1] : '';
  }

  private extractToobnixId(url: string): string {
    const match = url.match(/toobnix\.org\/w\/([^/?]+)/);
    return match ? match[1] : '';
  }
}

class TagsPickerRenderer {
  private store: StateStore;

  constructor(store: StateStore) {
    this.store = store;
  }

  render(headings: OrgHeading[]): RenderResult {
    let handlers: Array<{ nodeId: string, listenerName: string, handler: Function }> = [];
    if (headings.length === 0) {
      return {
        handlers: [],
        html: ''
      }
    }
    return {
      handlers: handlers,
      html: `<select id="filter" name="options" onchange="store.addFilterTag(this.value)">
               <option value="">tag</option>
                 ${Array.from(new Set(headings.flatMap(heading => heading.tags.map((tag) => tag.toLowerCase()) ?? [])))
                    .sort()
                    .map(tag => `<option value="${tag}">${tag}</option>`)
                    .join('')}
             </select>`
    }
  }
}

class DateRenderer {
  static render(dateStr?: string) {
    if (!dateStr) {
      return '';
    }
    const date = new Date(dateStr).toLocaleDateString('en-US', {
          year: 'numeric',
          month: 'long',
          day: 'numeric'
    })
    return `<span class="date">${date}</span>`;
  }
}

class VideoListRenderer {
  private store: StateStore;

  constructor(store: StateStore) {
    this.store = store;
  }

  render(headings: OrgHeading[]): RenderResult {
    const result = headings.reduce((acc, heading) => {
      const date = DateRenderer.render(heading.drawer.DATE);
      const title = heading.title || 'Untitled';

      const tags = heading.tags?.sort()?.map(tag => {
        tag = tag.toLowerCase();
        const tagId = `tag-${tag}-${randomUUID()}`;
        acc.handlers.push({
          nodeId: tagId,
          listenerName: 'click',
          handler: () => this.store.addFilterTag(tag)
        });
        return `<span id="${tagId}" class="tag">#${tag}</span>&nbsp;`;
      }).join(' ') || '';

      const links = Object.keys(heading.drawer ?? {})
        .filter(key => key.endsWith("_URL") && heading.drawer[key]?.trim())
        .map((key) => {
          return `<a href="${heading.drawer[key]}" target="_blank">${key.slice(0, -4).toLowerCase()}</a>`;
        })
        .join("&nbsp;·&nbsp;");

      const speakers = heading.drawer?.SPEAKERS;

      acc.html += `
      <div class="item">
        <p>${date}</p>
        <p><strong>${title}</strong></p>
        <p>${tags}</p>
${speakers ?
       `<p class="speakers">By ${speakers}</p>` : ''
}
        <p>${links}</p>
      </div><br>`;

      return acc;
    }, { html: '', handlers: [] });

    return result;
  }
}

class FilterByTagsRenderer {
  private store: StateStore;

  constructor(store: StateStore) {
    this.store = store;
  }

  render(tags: string[]): RenderResult {
    let handlers: Array<{ nodeId: string, listenerName: string, handler: Function }> = [];
    return {
      handlers: handlers,
      html: tags.map(tag => {
        const tagId = `filter-${tag}-${randomUUID()}`;
        handlers.push({
          nodeId: tagId,
          listenerName: 'click',
          handler: () => this.store.removeFilterTag(tag)
        });
        return `<span id="${tagId}" class="dismissible"><span class="tag">#${tag}</span>&nbsp;<span class="x">x</span></span>`;
      }).join(' ') || ''
    }
  }
}

export class StateStore {
  public state: ValueStream<State>;

  constructor() {
    this.state = new ValueStream({
      filterByTags: [],
      orgDocument: new OrgDocument([])
    });
  }

  public async load() {
    try {
      // Cloned from https://codeberg.org/sachac/emacs-videos/raw/branch/main/index.org
      const response = await fetch('./videos.org');
      if (!response.ok) {
        console.error('Could not fetch org feed');
        this.state.mutate(state => {
          state.orgDocument = new OrgDocument([]);
          return state;
        });
        return;
      }

      const text = await response.text();
      const orgDocument = OrgParser.parse(text);
      orgDocument.headings = orgDocument.headings.sort((a, b) => {
        const dateA = new Date(a.drawer?.DATE || 0);
        const dateB = new Date(b.drawer?.DATE || 0);
        return dateB.getTime() - dateA.getTime() || (!a.drawer?.DATE ? 1 : 0);
      });

      this.state.mutate(state => {
        state.orgDocument = orgDocument;
        return state;
      });
    } catch (error) {
      console.error(error);
      this.state.mutate(state => {
        state.error = error instanceof Error ? error.message : 'Unknown error occurred';
        return state;
      });
    }
  }

  public refresh() {
    this.state.mutate(state => {
      return state;
    });
  }

  public filterByTags(tagNames: string[]) {
    this.state.mutate(state => {
      state.filterByTags = tagNames;
      return state;
    });
  }

  public addFilterTag(tag: string) {
    this.state.mutate(state => {
      if (state.filterByTags.includes(tag.trim())) {
        return state;
      }
      state.filterByTags.push(tag.trim());
      return state;
    });
  }

  public removeFilterTag(tag: string) {
    this.state.mutate(state => {
      state.filterByTags = state.filterByTags.filter(filterTag => filterTag !== tag )
      return state;
    });
  }

  public resetFilter() {
    this.state.mutate(state => {
      state.filterByTags = [];
      return state;
    });
  }
}

export class OrgDocument {
  headings: OrgHeading[];
  constructor(headings: OrgHeading[]) {
    this.headings = headings;
  }
}

export class OrgHeading {
  title: string;
  tags: string[];
  drawer: Record<string, string>;

  constructor(title: string, tags: string[], drawer: Record<string, string>) {
    this.title = title;
    this.tags = tags;
    this.drawer = drawer;
  }
}

export class OrgParser {
  static parse(orgContent: string): OrgDocument {
    const lines = orgContent.split('\n');
    const headings: OrgHeading[] = [];
    let currentHeading: OrgHeading | null = null;

    for (const line of lines) {
      if (line.startsWith('* ')) {
        if (currentHeading) {
          headings.push(currentHeading);
        }

        let rawHeading = line.slice(2).trim(); // Remove the '* ' prefix
        let tags: string[] = [];

        // Match tags at the end of the line in the form :tag1:tag2:
        const tagMatch = rawHeading.match(/(:[^:\s]+)+:$/);
        if (tagMatch) {
          tags = tagMatch[0].split(':').filter(Boolean); // Extract tags and remove empty strings
          rawHeading = rawHeading.replace(/(:[^:\s]+)+:$/, '').trim(); // Remove tags from the heading
        }

        // The remaining string becomes the title
        const title = rawHeading;
        currentHeading = new OrgHeading(title, tags, {});
        continue;
      }

      // Detect properties inside ':PROPERTIES:' block
      if (line.startsWith(':') && currentHeading) {
        if (line.trim() === ':PROPERTIES:' || line.trim() === ':END:') {
          continue; // Skip property block markers
        }
        const propertyMatch = line.match(/^:([^:]+):\s*(.*)$/);
        if (propertyMatch) {
          const [, key, value] = propertyMatch;
          currentHeading.drawer[key.trim()] = value.trim();
        }
        continue;
      }
    }

    // Push the last heading if it exists
    if (currentHeading) {
      headings.push(currentHeading);
    }

    // Filter out any invalid or empty headings
    return new OrgDocument(headings.filter(heading => heading.title));
  }
}

export class ValueStore {
  private memoryStore: Map<string, string>;

  constructor() {
    this.memoryStore = new Map();
  }

  public getString(key: string): string | null {
    if (typeof window !== 'undefined' && window.localStorage) {
      return localStorage.getItem(key);
    } else {
      return this.memoryStore.get(key) || null;
    }
  }

  public setString(key: string, value: string): void {
    if (typeof window !== 'undefined' && window.localStorage) {
      localStorage.setItem(key, value);
    } else {
      this.memoryStore.set(key, value);
    }
  }

  public getObject<T>(key: string): T | null {
    const value = this.getString(key);
    return value ? JSON.parse(value) : null;
  }

  public setObject<T>(key: string, value: T): void {
    this.setString(key, JSON.stringify(value));
  }
}

export class ValueStream<T> {
  private subscribers: Set<(value: T) => void> = new Set();
  private currentValue: T | undefined;

  constructor(initialValue?: T) {
    if (initialValue !== undefined) {
      this.currentValue = initialValue;
    }
  }

  public subscribe(listener: (value: T) => void): () => void {
    this.subscribers.add(listener);
    if (this.currentValue !== undefined) {
      this.emit(this.currentValue);
    }
    return () => this.subscribers.delete(listener);
  }

  emit(value: T) {
    this.currentValue = value;
    this.subscribers.forEach(listener => listener(value));
  }

  clear() {
    this.subscribers.clear();
  }

  public mutate(mutator: (value: T) => T) {
    if (this.currentValue !== undefined) {
      const newValue = typeof this.currentValue === 'object' && this.currentValue !== null
       ? structuredClone(this.currentValue)
       : this.currentValue;
      this.currentValue = mutator(newValue);
      this.emit(this.currentValue);
    }
  }
}

interface RenderResult {
  html: string;
  handlers: Array<{ nodeId: string, listenerName: string, handler: Function }>;
}

export function makeStore(): StateStore {
  const store = new StateStore();
  store.state.mutate(state => {
    state = makeState();
    return state;
  });
  return store;
}

export function makeState(): State {
  return {
    filterByTags: [],
    orgDocument: new OrgDocument([])
  };
}

// TODO: Remove. This is sucky :)
function randomUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}
