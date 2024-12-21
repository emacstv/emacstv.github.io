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
  let idCounter = 1;
  const filterByTags = state.filterByTags.map(tag => {
    const tagId = `filter-${tag}-${idCounter}`;
    idCounter += 1;
    handlers.push({ nodeId: tagId, listenerName: 'click', handler: () => store.removeFilterTag(tag) });
    return `<span id="${tagId}" class="tag">${tag} <strong>x</strong></span>`;
  }).join(' ') || '';
let html = `
<h1>🦬 emacs.tv</h1>
<div>
 <select id="filter" name="options" onchange="store.addFilterTag(this.value)">
  <option value="">tag filter</option>
  ${Array.from(new Set(state.orgDocument.headings.flatMap(heading => heading.tags ?? [])))
      .sort()
      .map(tag => `<option value="${tag}">${tag}</option>`)
      .join('')}
 </select> ${filterByTags}
</div>
<br>
  `;
  handlers.push({ nodeId: 'filter', listenerName: 'onchange', handler: (tag: string) => store.removeFilterTag(tag) });

  state.orgDocument.headings
    .filter((heading) => {
      if (state.filterByTags.length === 0) {
        return true;
      }
      return state.filterByTags.every(filterTag => heading.tags.map((tag) => tag.toLowerCase()).includes(filterTag));
    })
    .forEach((heading) => {
      const date = heading.drawer?.DATE
        ? new Date(heading.drawer.DATE).toLocaleDateString('en-US', { year: 'numeric', month: 'long', day: 'numeric' })
        : '';
      const title = heading.title || 'Untitled';
      const tags = heading.tags?.sort()?.map(tag => {
        tag = tag.toLowerCase();
        const tagId = `tag-${tag}-${idCounter}`;
        idCounter += 1;
        handlers.push({ nodeId: tagId, listenerName: 'click', handler: () => store.addFilterTag(tag) });
        return `<span id="${tagId}" class="tag">${tag}</span>`;
      }).join(' ') || '';

      const links = Object.keys(heading.drawer ?? {})
        .filter(key => key.endsWith("_URL"))
        .map((key) => {
          return `<a href="${heading.drawer[key]}" target="_blank">${key.slice(0, -4).toLowerCase()}</a>`
        })
        .join("&nbsp;·&nbsp;")

      const speakers = heading.drawer?.SPEAKERS;
      html += `
      <div class="item">
        <p class="date">${date}</p>
        <p><strong>${title}</strong>&nbsp;${tags}</p>
        <p class="speakers">By ${speakers}</p>
        <p>${links}</p>
      </div><br>`;
    });
  return {
    html: state.error ? state.error : html,
    handlers: handlers
  };
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
      const response = await fetch('./index.org');
      if (!response.ok) {
        console.error('Could not fetch org feed');
        this.state.mutate(state => {
          state.orgDocument = new OrgDocument([]);
          return state;
        });
        return;
      }
      const text = await response.text();
      this.state.mutate(state => {
        const orgDocument = OrgParser.parse(text);
        orgDocument.headings = orgDocument.headings.sort((a, b) => {
          const dateA = new Date(a.drawer?.DATE || 0);
          const dateB = new Date(b.drawer?.DATE || 0);
          return dateB.getTime() - dateA.getTime() || (!a.drawer?.DATE ? 1 : 0);
        });
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
    for (let line of lines) {
      if (line.startsWith('* ')) {
        if (currentHeading){
          headings.push(currentHeading);
        }
        const [rest, tagsPart] = line.split(/ +:/);
        const tags = tagsPart ? tagsPart.split(':').filter((tag) => tag.trim() !== '') : [];
        const title = rest.replace(/^\* /, '').trim();
        currentHeading = new OrgHeading(title, tags, {});
        continue;
      }

      if (line.startsWith(':') && currentHeading) {
        const match = line.match(/^:([^:]+):\s*(.*)$/);
        if (line === ':PROPERTIES:' || line === ':END:') {
          continue;
        }
        if (match) {
          const [, key, value] = match;
          currentHeading.drawer[key] = value.trim();
        }
        continue;
      }
    }

    if (currentHeading) {
      headings.push(currentHeading)
    };

    return new OrgDocument(headings);
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
